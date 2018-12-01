open Context

type handler = Context.t -> (Cohttp.Response.t * Cohttp_lwt__Body.t) Lwt.t

type route = Re.re * handler

val routes : route array
