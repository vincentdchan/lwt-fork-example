open Core
open Cohttp
open Yojson
open Lwt_log

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
