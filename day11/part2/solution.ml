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

let move (x, y) = function
    S   -> (x,       succ y)
  | SE  -> (succ x,  y)
  | NE  -> (succ x,  pred y)
  | N   -> (x,       pred y)
  | NW  -> (pred x,  y)
  | SW  -> (pred x,  succ y)

let distance (ax, ay) (bx, by) =
  (abs(ax - bx)
   + abs(ax + ay - bx - by)
   + abs(ay - by)) / 2

let furthest_distance path =
  let advance (hex, dist) direction =
    let hex' = move hex direction in
    (hex', max dist (distance (0, 0) hex'))
  in
  List.fold path ~init:((0, 0), 0) ~f:advance |> snd

let _ =
  if not !Sys.interactive then
    match In_channel.input_line In_channel.stdin with
      Some input  -> String.split input ','
                     |> List.map ~f:direction_of_string
                     |> furthest_distance
                     |> string_of_int
                     |> print_endline
    | None        -> failwith "input"
