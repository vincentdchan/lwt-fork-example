open Lwt
open Lwt_io
open Lwt_log
open Cohttp
open Cohttp_lwt_unix
open Core
open Core_kernel

module Config = struct
  open Yojson
  type service = {
    name: string;
    work_dir: string;
    start_script: string;
  } [@@deriving yojson]

  type config = {
    slack_webhook: string;
    timeout: (float [@default 5.]);
    entries: service list;
  } [@@deriving yojson]

  let find_service_by_name { entries } name : service option =
    List.filter entries (
      fun service -> String.equal service.name name
    )
    |> List.hd
end

module Server = struct
  include Cohttp_lwt_unix.Server

  let respond_json ~body () =
    let headers = Header.init_with "Content-type" "application/json" in
    let body = body |> Yojson.Safe.to_string in
    respond_string ~headers ~status:`OK ~body ()

end

exception ContentTypeError of (string * string)

let assert_urlencoded_content_type req expect =
  let headers = req |> Request.headers in
  let content_type = Header.get headers "Content-Type"
                     |> (Option.value ~default:"not found")
  in
  if String.equal content_type expect then
    Lwt.return()
  else
    raise (ContentTypeError (expect, content_type))

module Context = struct
  type t = {
    request: Request.t;
    config: Config.config;
    body: Cohttp_lwt.Body.t;
    cli_mutex: Lwt_mutex.t;
  }
end

type handler = Context.t -> (Cohttp.Response.t * Cohttp_lwt__Body.t) Lwt.t

type route = string * handler

module Form_UrlEncoded = struct
  include Hashtbl

  exception FieldNotFound of string
  exception DuplicateKey of string

  let find_exn data name =
    match find data name with
    | Some value -> value
    | None -> raise (FieldNotFound name)

  let add_exn data key value =
    match add data key value with
    |`Duplicate -> raise (DuplicateKey key)
    |`Ok -> ()

  let create () = Hashtbl.create (module String)

  let from_string (content: string) =
    let data = String.split ~on:'&' content in
    let table = create () in

    List.fold_until
      data
      ~init:table
      ~f:(fun acc str ->
        match String.split ~on:'=' str with
        | key::value::[] ->
            add_exn acc key value;
            Continue table
        | _ ->
          Stop table
      )
      ~finish:(fun result -> result)

  let from_tuples tuples =
    let result = create () in

    List.iter tuples ~f:(
      fun (key, value) -> add_exn result key value
    );

    result

end

module Slack = struct
  type error = {
    response_type: string;
    text: string;
  } [@@deriving yojson]

  module Response = struct

    type message = {
      text: string;
    } [@@deriving yojson]

    type command_resp = {
        response_type: string;
        text: string;
        attachments: message list;
    } [@@deriving yojson]

  end
end


(* send all the services to Slack *)
let bot_list_all_repos_handler = (
  "/command/list",
  fun { Context. request; config; cli_mutex } ->
    let open Slack.Response in
    let open Config in

    let pid = Lwt_unix.fork () in

    if pid <> 0 then
      let%lwt attachments =
        Lwt_list.map_p
          (fun { name; work_dir } ->
            Lwt_mutex.with_lock cli_mutex (fun () ->
              let open Config in

              let text = "something" in
              Lwt.return { text }
              )
          )
          config.entries
      in
      let resp =
        {
          response_type = "in_channel";
          text = "List of all repos:";
          attachments;
        }
      in
      let body = resp |> Slack.Response.command_resp_to_yojson in
      Server.respond_json ~body ()
    else
      begin
        printf "pid: %d" pid;
        let fst = List.hd_exn config.entries in
        let headers = Header.add
            (Header.init ())
            "Content-Type" "application/json"
        in
        let body =
          Slack.Response.(
            {
              text =
                Printf.sprintf "Repo %s has been updated by %s" fst.name "xixi";
            }
            |> message_to_yojson
            |> Yojson.Safe.to_string
            |> Cohttp_lwt.Body.of_string
          )
        in
        let%lwt _ = Client.post ~body ~headers (Uri.of_string config.slack_webhook) in
        Pervasives.exit(0)
      end

)


let routes: route array = [|
  bot_list_all_repos_handler;
|]

let log_request req status =
  let open Unix in
  let uri = req |> Request.uri |> Uri.to_string in
  let meth = req |> Request.meth |> Code.string_of_method in
  info_f "%s %s %s" status meth uri

let server port =
  let content = Yojson.Safe.from_file "config.json" in
  let config = Config.config_of_yojson content |> Result.ok_or_failwith in

  let timeout () =
    let%lwt () = Lwt_unix.sleep config.timeout in
    Lwt.return None
  in

  let callback _conn req body =

    let path = req |> Request.uri |> Uri.path in

    let handler_opt : handler option = Array.fold_until
      routes
      ~init:None
      ~f: (fun _ (path_reg, hand) ->
          if String.equal path_reg path then
            Stop (Some hand)
          else
            Continue None
        )
      ~finish: (fun opt -> opt)
    in

    let open Context in
    let%lwt resp, body = match handler_opt with
    | Some hand ->
      begin
        try%lwt
          let handler_tranch =
            hand { request = req; config; body; cli_mutex = Lwt_mutex.create () }
            >|= fun result ->
            (Some result)
          in

          match%lwt Lwt.pick [ handler_tranch; timeout () ] with
          | Some result ->
            Lwt.return result
          | None ->
            Server.respond_error
              ~status:`Internal_server_error
              ~body:"Internal Error: server timeout"
              ()

        with
        | ContentTypeError (expect, actual) ->
          let%lwt () = error_f "Unexpected exception: %s, expected: %s" actual expect in
          Server.respond_error
            ~status:`Internal_server_error
            ~body:"unexpected Content-Type"
            ()

        | e ->
          let msg = Exn.to_string e in
          let stack = Printexc.get_backtrace () in
          let%lwt () = error_f ~exn:e "There was an error: %s\n%s]n" msg stack in
          Server.respond_error
            ~status:`Internal_server_error
            ~body:"Unknown internal Error"
            ()
      end
    | None ->
      Server.respond_error
        ~status:`Not_found
        ~body:"page not found"
        ()
    in
    let status = resp |> Cohttp.Response.status |> Code.string_of_status in
    log_request req status >>= fun () ->
    Lwt.return (resp, body)

  in

  let%lwt _ = Lwt_io.printf "Start server at localhost:%d...\n" port in
  Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())

let command =
  Command.basic
    ~summary:"God's in his heaven, all right with the world"
    Command.Let_syntax.(
      let%map_open
        verbose = flag "-verbose" no_arg
          ~doc:"bool Determine if show verbose logs"
      in
      fun () ->
        if verbose then
          begin
            Lwt_log.default :=
              Lwt_log.channel
                ~template:"$(date).$(milliseconds) [$(level)] $(message)"
                ~close_mode:`Keep
                ~channel:Lwt_io.stdout
                ();

            Lwt_log.add_rule "*" Lwt_log.Info;
          end
        else
          ()
    )

let () =
  Command.run ~version:"1.0" command;
  ignore (Lwt_main.run (server 8000))
