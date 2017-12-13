(* Day 11: Hex Ed *)

(* ocamlbuild -use-ocamlfind -pkgs core -tag thread solution.native *)
(* ./solution.native < <input-file> *)

open Core

type direction = S | SE | NE | N | NW | SW

let direction_of_string = function
    "s"  -> S
  | "se" -> SE
  | "ne" -> NE
  | "n"  -> N
  | "nw" -> NW
  | "sw" -> SW
  | _    -> failwith "direction_of_string"

type hex = {x: int; y: int}

let origin = {x=0; y=0}

let move from = function
    S   -> {x = from.x;       y = succ from.y}
  | SE  -> {x = succ from.x;  y = from.y}
  | NE  -> {x = succ from.x;  y = pred from.y}
  | N   -> {x = from.x;       y = pred from.y}
  | NW  -> {x = pred from.x;  y = from.y}
  | SW  -> {x = pred from.x;  y = succ from.y}

let directions = [S; SE; NE; N; NW; SW]

let neighbours from = List.map directions (move from)

let distance a b =
  (abs(a.x - b.x)
   + abs(a.x + a.y - b.x - b.y)
   + abs(a.y - b.y)) / 2

let actual_distance path =
  let end_position = List.fold path ~init:origin ~f:move in
  distance origin end_position

let _ =
  if not !Sys.interactive then
    match In_channel.input_line In_channel.stdin with
      Some input  -> String.split input ','
                     |> List.map ~f:direction_of_string
                     |> actual_distance
                     |> string_of_int
                     |> print_endline
    | None        -> failwith "input"

let tests = [([NE;NE;NE],        3);
             ([NE;NE;SW;SW],     0);
             ([NE;NE;S;S],       2);
             ([SE;SW;SE;SW;SW],  3)]

let _ =
  if !Sys.interactive then
    let _ = print_endline "Running tests..." in
    let _ =
      List.iter tests (fun (path, expected) ->
                  print_endline (if actual_distance path = expected then "PASSED" else "FAILED"))
    in
    print_endline "Running tests...DONE"
