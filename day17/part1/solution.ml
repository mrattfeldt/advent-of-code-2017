(* Day 1: Spinlock *)

(* ocamlbuild -use-ocamlfind -pkgs core -tag thread solution.native *)
(* ./solution.native *)

open Core

let rec list_insert i y = function
    x :: xs when i = 0 -> x :: y :: xs
  | x :: xs when i > 0 -> x :: (list_insert (pred i) y xs)
  | _                  -> failwith "list_insert"

let spinlock steps =
  let repeats = 2017 in
  let rec spinlock buffer i n =
    if n = repeats then List.nth_exn buffer (succ i)
    else
      let n1 = succ n in
      let i1 = (i + steps) mod n1 in
      spinlock (list_insert i1 n1 buffer) (succ i1) n1
  in
  spinlock [0] 0 0

let _ =
  if not !Sys.interactive then
    spinlock 328
    |> string_of_int
    |> print_endline
