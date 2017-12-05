(* Day 3: Spiral Memory *)

(* Build with 'ocamlopt str.cmxa puzzle.ml -o puzzle' *)
(* Run tests with './puzzle test' *)
(* Run with './puzzle < <input-file>' *)

open List

exception Error

module Coord =
  struct
    type t = int * int
    let compare (x0,y0) (x1,y1) =
      match Pervasives.compare x0 x1 with
        0 -> Pervasives.compare y0 y1
      | c -> c
  end

module CoordSet = Set.Make(Coord)

let spiral_dist data =
  let go_left (x0, y0) (x1, y1) =
    if      x0 = pred x1 then  (x1, succ y1)
    else if y0 = pred y1 then  (pred x1, y1)
    else if x0 = succ x1 then  (x1, pred y1)
    else if y0 = succ y1 then  (succ x1, y1)
    else raise Error in
  let go_straight (x0, y0) (x1, y1) =
    if x0 = pred x1      then  (succ x1, y1)
    else if y0 = pred y1 then  (x1, succ y1)
    else if x0 = succ x1 then  (pred x1, y1)
    else if y0 = succ y1 then  (x1, pred y1)
    else raise Error in
  let rec find_coord seen index previous current =
    if index = data then
      current
    else
      let left = go_left previous current in
      if CoordSet.mem left seen then
        let straight = go_straight previous current in
        find_coord (CoordSet.add straight seen) (succ index) current straight
      else
        find_coord (CoordSet.add left seen) (succ index) current left
  in
  if data = 1 then
    0
  else
    let (x_dist, y_dist) = find_coord (CoordSet.of_list [(0, 0); (1, 0)]) 2 (0, 0) (1, 0) in
    abs x_dist + abs y_dist

let tests = [(1, 0); (12, 3); (23, 2); (1024, 31)]

let _ =
  if Array.length Sys.argv > 1 &&  Sys.argv.(1) = "test" then
    List.iter (fun (data, verification) ->
        let result = spiral_dist data in
        if result <> verification then
          print_endline (string_of_int data ^ " is carried " ^ string_of_int result
                         ^ " steps, but should be carried " ^ string_of_int verification ^ " steps"))
              tests
  else
    let data = int_of_string (read_line ()) in
    print_endline (string_of_int (spiral_dist data))
