(* Day 10: Knot Hash *)

(* ocamlbuild -use-ocamlfind -pkgs core -tag thread solution.native *)
(* ./solution.native < <input-file> *)

open Core

type state = {circle: int list; pos: int; skip: int}

exception Error

let sublist pos len = List.filteri ~f:(fun i e -> pos <= i && i < pos + len)

let string_to_ascii_list s = List.init (String.length s) (fun i -> Char.to_int s.[i])

let range n = List.init n (fun i -> i)

let sparse_hash input =
  let knot lengths state =
    let n = List.length state.circle in
    let rec advance s length =
      let circle' =
        if s.pos + length < n then
          let prefix    = sublist 0 s.pos s.circle 
          and reversed  = sublist s.pos length s.circle |> List.rev 
          and suffix    = sublist (s.pos + length) (n - (s.pos + length)) s.circle  in
          prefix @ reversed @ suffix
        else
          let prefix_len = (s.pos + length) mod n
          and middle_len = s.pos - ((s.pos + length) mod n)
          and suffix_len = n - s.pos in
          let prefix     = sublist 0 prefix_len s.circle 
          and middle     = sublist prefix_len middle_len s.circle 
          and suffix     = sublist s.pos suffix_len s.circle in
          let reversed   = suffix @ prefix |> List.rev in
          let suffix'    = List.take reversed suffix_len 
          and prefix'    = List.drop reversed suffix_len in
          prefix' @ middle @ suffix'
      in
      {circle = circle'; pos = (s.pos + length + s.skip) mod n; skip = succ s.skip}
    in
    (List.fold lengths ~init:state ~f:advance)
  in
  let lengths = string_to_ascii_list input @ [17; 31; 73; 47; 23] in
  let initial_state = {circle = range 256; pos = 0; skip = 0} in
  (List.fold (range 64) ~init:initial_state ~f:(fun ack _ -> knot lengths ack)).circle
  
let dense_hash sparse_hash =
  let rec split_blocks = function
      [] -> []
    | xs -> List.take xs 16 :: (split_blocks (List.drop xs 16))
  in
  split_blocks sparse_hash
  |> List.map ~f:(List.fold ~init:0 ~f:(lxor))

let knot_hash input =
  (sparse_hash input)
  |> dense_hash
  |> List.map ~f:(Printf.sprintf "%02x")
  |> List.fold ~init:"" ~f:(^)
