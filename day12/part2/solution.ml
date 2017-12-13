(* Day 12: Digital Plumber *)

(* ocamlbuild -use-ocamlfind -pkgs core,str -tag thread solution.native *)
(* ./solution.native < <input-file> *)

open Core

let (>>) f g x = g(f(x))

module Connections = Map.Make(Int)
module IntSet = Set.Make(Int)

let group prog connections =
  let rec collect seen prog =
    Connections.find_exn connections prog
    |> List.filter ~f:(fun p -> not (IntSet.mem seen p))
    |> List.fold ~init:seen ~f:(fun ack p -> collect (IntSet.add ack p) p)
  in
  collect IntSet.empty prog

let count_groups connections =
  let rec count_groups ack remaining =
    match IntSet.choose remaining with
      None   -> ack
    | Some p -> IntSet.diff remaining (group p connections)
                |> count_groups (succ ack)
  in
  count_groups 0 (Connections.keys connections |> IntSet.of_list)

let extract_data lines =
  let extract_line line =
    match Str.split (Str.regexp "[ \t]+") line with
      prog :: _double_arrow :: progs ->
      (int_of_string prog, List.map progs ((String.strip ~drop:((=)',')) >> int_of_string))
    | _ -> failwith "input"
  in
  Connections.of_alist_exn (List.map lines extract_line)

let _ =
  if not !Sys.interactive then
    In_channel.input_lines In_channel.stdin
    |> extract_data
    |> count_groups
    |> string_of_int
    |> print_endline

(* Test code below *)

let tests = [(["0 <-> 2";
               "1 <-> 1";
               "2 <-> 0, 3, 4";
               "3 <-> 2, 4";
               "4 <-> 2, 3, 6";
               "5 <-> 6";
               "6 <-> 4, 5"], 2)]

let _ =
  if !Sys.interactive then
    let _ = print_endline "Running tests..." in
    let _ =
      List.iter tests (fun (input, expected) ->
                  let result = extract_data input |> count_groups in
                  print_endline (if result = expected then "PASSED" else "FAILED")) in
    print_endline "Running tests...DONE"
