(* Day 14: Disk Defragmentation *)

(* ocamlbuild -use-ocamlfind -pkgs core,str -tag thread solution.native *)
(* ./solution.native < <input-file> *)

open Core

let bit_of_hex hex =
  let zero_pad s = match String.length s with 4 -> s | n -> (String.make (4 - n) '0') ^ s in
  let int_of_hex hex = int_of_string ("0x" ^ (Char.escaped hex)) in
  let rec bit_of_int n = 
    let (n2, bit) = (n/2, n mod 2 |> string_of_int) in
    if n2 = 0 then bit else (bit_of_int n2) ^ bit
  in
  String.fold ~init:"" ~f:(fun ack h -> ack ^ (bit_of_int (int_of_hex h) |> zero_pad)) hex

let count key = 
  List.init 128 (fun i -> key ^ "-" ^ string_of_int i)
  |> List.map ~f:Knot.knot_hash
  |> List.map ~f:bit_of_hex
  |> List.fold ~init:0 ~f:(fun ack s -> ack + String.count s ((=)'1'))
                  
let _ =
  if not !Sys.interactive then
    match In_channel.input_line In_channel.stdin with
      Some key ->  count key
                   |> string_of_int
                   |> print_endline
    | None     -> failwith "input"
