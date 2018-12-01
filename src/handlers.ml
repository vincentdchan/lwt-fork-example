open Lwt
open Context
open Core_kernel
open Cohttp
open Lwt_log
open Cohttp_lwt_unix
open Util

type handler = Context.t -> (Cohttp.Response.t * Cohttp_lwt__Body.t) Lwt.t

type route = Re.re * handler

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

(* send all the services to Slack *)
let bot_list_all_repos_handler = (
  Re.Pcre.regexp "^/command/list$",
  fun { request; config; cli_mutex } ->
    let open Slack.Response in
    let open Config in

    let pid = Lwt_unix.fork () in

    if pid <> 0 then
      let%lwt () = assert_urlencoded_content_type request "application/x-www-form-urlencoded" in
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
