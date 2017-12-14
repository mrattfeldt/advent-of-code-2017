(* Day 14: Disk Defragmentation *)

(* ocamlbuild -use-ocamlfind -pkgs core,str -tag thread solution.native *)
(* ./solution.native < <input-file> *)

open Core

module Adjacent = Map.Make(Int)
module IntSet = Set.Make(Int)

let bit_of_hex hex =
  let zero_pad s = match String.length s with 4 -> s | n -> (String.make (4 - n) '0') ^ s in
  let int_of_hex hex = int_of_string ("0x" ^ (Char.escaped hex)) in
  let rec bit_of_int n = 
    let (n2, bit) = (n/2, n mod 2 |> string_of_int) in
    if n2 = 0 then bit else (bit_of_int n2) ^ bit
  in
  String.fold ~init:"" ~f:(fun ack h -> ack ^ (bit_of_int (int_of_hex h) |> zero_pad)) hex

let grid_of_key key =
  List.init 128 (fun i -> key ^ "-" ^ string_of_int i)
  |> List.map ~f:Knot.knot_hash
  |> List.map ~f:bit_of_hex
  |> List.fold ~init:"" ~f:(^)

let neighbours_of grid square =
    let (x, y) = (square mod 128, square / 128) in
    [(x, pred y); (succ x, y); (x, succ y); (pred x, y)]
    |> List.filter ~f:(fun (x, y) -> 0 <= x && x < 128 && 0 <= y && y < 128) (* valid *)
    |> List.map ~f:(fun (x, y) -> y * 128 + x)
    |> List.filter ~f:(fun n -> grid.[n] = '1')                              (* filled *)

let adjacent grid =
  List.init (String.length grid)
            (fun s -> if grid.[s] = '0' then [] else [(s, s :: (neighbours_of grid s))])
  |> List.concat
  |> Adjacent.of_alist_exn

let region_of adjacent square =
  let rec collect seen square =
    Adjacent.find_exn adjacent square
    |> List.filter ~f:(fun n -> not (IntSet.mem seen n))
    |> List.fold ~init:seen ~f:(fun ack n -> collect (IntSet.add ack n) n)
  in
  collect IntSet.empty square

let count_regions grid =
  let adjacent = adjacent grid in
  let rec count_regions ack remaining =
    match IntSet.choose remaining with
      None   -> ack
    | Some p -> IntSet.diff remaining (region_of adjacent p)
                |> count_regions (succ ack)
  in
  count_regions 0 (Adjacent.keys adjacent |> IntSet.of_list)

let _ =
  if not !Sys.interactive then
    match In_channel.input_line In_channel.stdin with
      Some key -> grid_of_key key
                  |> count_regions
                  |> string_of_int
                  |> print_endline
    | None     -> failwith "input"
