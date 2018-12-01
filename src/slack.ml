

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

