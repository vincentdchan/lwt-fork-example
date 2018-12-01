open Cohttp

type t = {
  request: Request.t;
  config: Config.config;
  body: Cohttp_lwt.Body.t;
  cli_mutex: Lwt_mutex.t;
}
