(* Day 15: Dueling Generators *)

(* ocamlbuild solution.native *)
(* ./solution.native *)

let rec count ack a b n =
  let matches a b = a land 0xffff = b land 0xffff in
  let rec next factor multiple x =
    let x' = (x * factor) mod 2147483647 in
    if x' mod multiple = 0 then x' else next factor multiple x' in
  let (nexta, nextb) = (next 16807 4, next 48271 8) in
  if n = 0 then ack
  else
    let na, nb = nexta a, nextb b in
    count (if matches na nb then succ ack else ack) na nb (pred n)

let starta, startb = 116, 299

let _ =
  if not !Sys.interactive then
    count 0 starta startb 5000000
    |> string_of_int
    |> print_endline
