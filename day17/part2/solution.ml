(* Day 1: Spinlock *)

(* ocamlbuild solution.native *)
(* ./solution.native *)

let spinlock steps =
  let repeats = 50000000 in
  let rec spinlock ack i n =
    if n = repeats then ack
    else
      let n1 = succ n in
      let i1 = (i + steps) mod n1 in
      spinlock (if i1 = 0 then n1 else ack) (succ i1) n1
  in
  spinlock 0 0 0

let _ =
  if not !Sys.interactive then
    spinlock 328
    |> string_of_int
    |> print_endline
