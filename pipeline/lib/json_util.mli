val parse_many : string -> Yojson.Safe.t list

val validate_json : Yojson.Safe.t list -> (string option, Yojson.Safe.t list) Hashtbl.t