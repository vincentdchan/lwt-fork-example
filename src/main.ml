open Lwt
open Lwt_io
open Lwt_log
open Cohttp
open Cohttp_lwt_unix
open Core
open Core_kernel
open Util

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

    let open Handlers in

    let handler_opt : handler option = Array.fold_until
      routes
      ~init:None
      ~f: (fun _ (path_reg, hand) ->
          if Re.Pcre.pmatch path_reg path then
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
