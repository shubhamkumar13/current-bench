let stream_to_list stream =
  let acc = ref [] in
  Stream.iter (fun x -> acc := x :: !acc) stream;
  List.rev !acc

let parse_many string = stream_to_list (Yojson.Safe.stream_from_string string)

open Current.Syntax
module Re = Pcre

let get_benchmark_name json =
  json |> Yojson.Safe.Util.(member "name") |> Yojson.Safe.Util.to_string_option

let get_result_list json =
  json |> Yojson.Safe.Util.(member "results") |> Yojson.Safe.Util.to_list

let validate_json json_list =
  let tbl = Hashtbl.create 1000 in
  List.iter
    (fun json ->
      let benchmark_name = get_benchmark_name json in
      match Hashtbl.find_opt tbl benchmark_name with
      | Some _ ->
          raise
          @@ Failure
               "This benchmark name already exists, please create a unique name"
      | None ->
          let results = get_result_list json in
          Hashtbl.add tbl benchmark_name results)
    json_list;
  tbl

let get_matches rex str =
  Re.exec_all ~rex str
  |> Array.map @@ Re.get_opt_substrings ~full_match:true

let get_complete_match rex str =
  let arr = get_matches rex str in
  (* first match is the substring with absolute regexp match *)
  match arr.(0).(0) with
  | None -> raise @@ Failure "no complete match found"
  | Some s -> s

let check_pread_log (s : string Current.t) : string Current.t = 
  let* s = s in
  let r = Re.regexp {|\{(?:[^{}]|(\{(?:[^{}]|(\{(?:[^{}]|())+\}))+\}))+\}|} in
  Current.return (get_complete_match r s)