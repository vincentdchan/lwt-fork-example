open Lwt
open Lwt_io
open Lwt_log
open Cohttp
open Cohttp_lwt_unix
open Core
open Core_kernel
open Cohttp_lwt_unix.Server

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
    body: Cohttp_lwt.Body.t;
    cli_mutex: Lwt_mutex.t;
  }
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

let hand { Context. request; cli_mutex } =
  let open Slack.Response in

  let pid = Lwt_unix.fork () in

  if pid <> 0 then
    respond_string ~status:`OK ~body:"hello world" ()
  else
    begin
      printf "pid: %d" pid;
      (* let fst = List.hd_exn config.entries in *)
      let headers = Header.add
          (Header.init ())
          "Content-Type" "application/json"
      in
      let body =
        Slack.Response.(
          {
            text = "Repo a has been updated by xixi";
          }
          |> message_to_yojson
          |> Yojson.Safe.to_string
          |> Cohttp_lwt.Body.of_string
        )
      in
      let%lwt _ = Client.post ~body ~headers (Uri.of_string "https://www.google.com") in
      Pervasives.exit(0)
    end


let log_request req status =
  let open Unix in
  let uri = req |> Request.uri |> Uri.to_string in
  let meth = req |> Request.meth |> Code.string_of_method in
  info_f "%s %s %s" status meth uri

let server port =
  let timeout () =
    let%lwt () = Lwt_unix.sleep 5. in
    Lwt.return None
  in

  let callback _conn req body =
    let open Context in
    let%lwt resp, body =
      begin
        try%lwt
          let handler_tranch =
            hand { request = req; body; cli_mutex = Lwt_mutex.create () }
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
    in
    let status = resp |> Cohttp.Response.status |> Code.string_of_status in
    log_request req status >>= fun () ->
    Lwt.return (resp, body)

  in

  let%lwt _ = Lwt_io.printf "Start server at localhost:%d...\n" port in
  Server.create ~mode:(`TCP (`Port port)) (Server.make ~callback ())

let () =
  ignore (Lwt_main.run (server 8000))
