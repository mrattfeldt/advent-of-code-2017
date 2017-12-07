(* Day 5: A Maze of Twisty Trampolines, All Alike *)

(* ocamlbuild -use-ocamlfind -pkgs core -tag thread solution.native *)
(* ./solution.native < <input-file> *)

open Core

let steps maze =
  let rec loop index count =
    if 0 <= index && index < Array.length maze then
      let next_index = index + maze.(index) in
      let _ = maze.(index) <- succ maze.(index) in
      loop next_index (succ count)
    else
      count
  in
  loop 0 0

let _ =
  if not !Sys.interactive then
    let maze = List.map (In_channel.input_lines In_channel.stdin) int_of_string |> Array.of_list in
    print_endline (string_of_int (steps maze))

(* Test code below *)

let tests = [([|0; 3; 0; 1; -3|], 5)]

let _ =
  if !Sys.interactive then
    let _ = print_endline "Running tests..." in
    let _ = List.iter tests (fun (instance, expected) ->
                        print_endline (if steps instance = expected then "PASSED" else "FAILED")) in
    print_endline "Running tests...DONE"
