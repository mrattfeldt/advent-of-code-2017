(* Day 9: Stream Processing *)

(* ocamlbuild -use-ocamlfind -pkgs core -tag thread solution.native *)
(* ./solution.native < <input-file> *)

open Core

type state = {level: int; garbage: bool; cancel: bool; score: int}
               
let score stream =
  let rec advance s = function
      '{' when s.garbage || s.cancel -> {s with cancel = false}
    | '{'                            -> {s with level = succ s.level; score = s.score + succ s.level}
    | '}' when s.garbage || s.cancel     -> {s with cancel = false}
    | '}'                                -> {s with level = pred s.level}
    | '<' when s.garbage || s.cancel     -> {s with cancel = false}
    | '<'                                -> {s with garbage = true}
    | '>' when s.garbage && not s.cancel -> {s with garbage = false}
    | '!' when s.garbage                 -> {s with cancel = not s.cancel}
    | _                                  -> {s with cancel = false}
  in
  let initial_state = {level = 0; garbage = false; cancel = false; score = 0} in
  (List.fold stream ~init:initial_state ~f:advance).score

let _ =
  if not !Sys.interactive then
    match In_channel.input_line In_channel.stdin with
      Some input -> String.to_list input |> score |> string_of_int |> print_endline
    | None -> print_endline "error"

let tests = [("{}",                              1);
             ("{{{}}}",                          6);
             ("{{},{}}",                         5);
             ("{{{},{},{{}}}}",                 16);
             ("{<a>,<a>,<a>,<a>}",               1);
             ("{{<ab>},{<ab>},{<ab>},{<ab>}}",   9);
             ("{{<!!>},{<!!>},{<!!>},{<!!>}}",   9);
             ("{{<a!>},{<a!>},{<a!>},{<ab>}}",   3)]

let _ =
  if !Sys.interactive then
    let _ = print_endline "Running tests..." in
    let _ =
      List.iter tests (fun (input, expected) ->
                  let score = String.to_list input |> score in
                  print_endline (if score = expected then "PASSED" else "FAILED"))
    in
    print_endline "Running tests...DONE"
