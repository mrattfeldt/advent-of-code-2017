(* Day 9: Stream Processing *)

(* ocamlbuild -use-ocamlfind -pkgs core -tag thread solution.native *)
(* ./solution.native < <input-file> *)

open Core

type state = {garbage: bool; cancel: bool; count: int}
               
let count stream =
  let rec advance s c =
    let s' = match c with
        '{' when s.garbage || s.cancel     -> {s with cancel = false}
      | '}' when s.garbage || s.cancel     -> {s with cancel = false}
      | '<' when s.garbage || s.cancel     -> {s with cancel = false}
      | '<'                                -> {s with garbage = true}
      | '>' when s.garbage && not s.cancel -> {s with garbage = false}
      | '!' when s.garbage                 -> {s with cancel = not s.cancel}
      | _                                  -> {s with cancel = false} in
    if s.garbage && s'.garbage && not s.cancel && not s'.cancel then {s' with count = succ s.count}
    else s'
  in
  let initial_state = {garbage = false; cancel = false; count = 0} in
  (List.fold stream ~init:initial_state ~f:advance).count

let _ =
  if not !Sys.interactive then
    match In_channel.input_line In_channel.stdin with
      Some input -> String.to_list input |> count |> string_of_int |> print_endline
    | None -> print_endline "error"

let tests = [("<>",                   0);
             ("<random characters>",  17);
             ("<<<<>",                3);
             ("<{!>}>",               2);
             ("<!!>",                 0);
             ("<!!!>>",               0);
             ("<{o\"i!a,<{i<a>",     10)]

let _ =
  if !Sys.interactive then
    let _ = print_endline "Running tests..." in
    let _ =
      List.iter tests (fun (input, expected) ->
                  let count = String.to_list input |> count in
                  print_endline (if count = expected then "PASSED" else "FAILED"))
    in
    print_endline "Running tests...DONE"
