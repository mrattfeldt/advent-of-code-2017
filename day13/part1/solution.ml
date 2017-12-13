(* Day 13: Packet Scanners *)

(* ocamlbuild -use-ocamlfind -pkgs core,str -tag thread solution.native *)
(* ./solution.native < <input-file> *)

open Core

type layer = {depth: int; range: int; scanner: int; tick: int -> int}

let init_layer depth range = {depth = depth; range = range; scanner = 1; tick = succ}

let empty_layer depth = {depth = depth; range = 0; scanner = 0; tick = Fn.id}

let tick layer =
  let layer' = {layer with scanner = layer.tick layer.scanner} in
  if layer'.scanner = layer.range then {layer' with tick = pred}
  else if layer'.scanner = 1 then {layer' with tick = succ}
  else layer'

let severity layers =
  let rec severity ack = function
      []      -> ack
    | l :: ls -> List.map ls tick
                 |> severity (ack + if l.scanner = 1 then l.depth * l.range else 0)
  in
  severity 0 layers

let extract_data lines =
  let extract_line line =
    match Str.split (Str.regexp "[ \t]+") line with
      [depth; range] ->
      (String.strip ~drop:((=)':') depth |> int_of_string, int_of_string range)
    | _ -> failwith "input"
  in
  let layers = List.map lines extract_line in
  List.init (List.last layers |> Option.value ~default:(0, 0) |> fst |> succ)
            (fun d -> match List.Assoc.find layers ~equal:(=) d with
                        Some range -> init_layer d range
                      | None       -> empty_layer d)

let _ =
  if not !Sys.interactive then
    In_channel.input_lines In_channel.stdin
    |> extract_data
    |> severity
    |> string_of_int
    |> print_endline

(* Test code below *)

let tests = [(["0: 3";
               "1: 2";
               "4: 4";
               "6: 4"], 24)]

let _ =
  if !Sys.interactive then
    let _ = print_endline "Running tests..." in
    let _ =
      List.iter tests (fun (input, expected) ->
                  print_endline (if extract_data input |> severity = expected
                                 then "PASSED"
                                 else "FAILED")) in
    print_endline "Running tests...DONE"
