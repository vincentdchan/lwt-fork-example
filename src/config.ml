open Core_kernel
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
