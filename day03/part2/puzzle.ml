(* Day 3: Spiral Memory *)

(* Build with 'ocamlopt puzzle.ml -o puzzle' *)
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

module CoordMap = Map.Make(Coord)

let stress_test value =
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
  let calculate_sum (x, y) coord_sum =
    fold_left
      (fun ack coord ->
        if CoordMap.mem coord coord_sum then ack + (CoordMap.find coord coord_sum) else ack)
      0 [(x, pred y); (succ x, pred y); (succ x, y); (succ x, succ y);
         (x, succ y); (pred x, succ y); (pred x, y); (pred x, pred y)]
  in
  let rec next_higher_value coord_sum previous current =
    let current_sum = CoordMap.find current coord_sum in
    if current_sum > value then
      current_sum
    else
      let left = go_left previous current in
      let turn = if CoordMap.mem left coord_sum then go_straight previous current else left in
      let sum = calculate_sum turn coord_sum in
      let next_coord_sum = CoordMap.add turn sum coord_sum in
      next_higher_value next_coord_sum current turn
  in
  if value = 1 then 2
  else
    let coord_sum = CoordMap.add (1, 0) 1 (CoordMap.add (0, 0) 1 CoordMap.empty) in
    next_higher_value coord_sum (0, 0) (1, 0)

let tests = [(1, 2); (2, 4); (10, 11); (747, 806)]

let _ =
  if Array.length Sys.argv > 1 &&  Sys.argv.(1) = "test" then
    List.iter (fun (value, verification) ->
        let result = stress_test value in
        if result <> verification then
          print_endline ("the next larger value of " ^ string_of_int value
                         ^ " is " ^ string_of_int result
                         ^ ", but should be " ^ string_of_int verification))
              tests
  else
    let value = int_of_string (read_line ()) in
    print_endline (string_of_int (stress_test value))
