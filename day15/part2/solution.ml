(* Day 15: Dueling Generators *)

(* ocamlbuild solution.native *)
(* ./solution.native *)

let rec count ack a b n =
  let matches a b =
    let mask = (1 lsl 16) - 1 in
    a land mask = b land mask
  in
  let rec next factor multiple x =
    let x' = (x * factor) mod 2147483647 in
    if x' mod multiple = 0 then x' else next factor multiple x'
  in
  let (nexta, nextb) = (next 16807 4, next 48271 8) in
  match n with
    0  -> ack
  | n  ->
     let (na, nb) = (nexta a, nextb b) in
     count (if matches na nb then succ ack else ack) na nb (pred n)
                
let start_a = 116
let start_b = 299

let _ =
  if not !Sys.interactive then
    count 0 start_a start_b 5000000
    |> string_of_int
    |> print_endline
