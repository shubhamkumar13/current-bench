let stream_to_list stream =
  let acc = ref [] in
  Stream.iter (fun x -> acc := x :: !acc) stream;
  List.rev !acc

let parse_many string = stream_to_list (Yojson.Safe.stream_from_string string)

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