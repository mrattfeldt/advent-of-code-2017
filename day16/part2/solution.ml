(* Day 16: Permutation Promenade *)

(* ocamlbuild -use-ocamlfind -pkgs core -tag thread solution.nativeo *)
(* ./solution.native < <input-file> *)

open Core

type move = Spin of int | Exchange of int * int | Partner of char * char

let move_of_string = function
    s when s.[0] = 's' -> Spin (String.suffix s (pred (String.length s)) |> int_of_string)
  | s when s.[0] = 'x' ->
     (match String.split (String.suffix s (pred (String.length s))) '/' with
        [a; b] -> Exchange (int_of_string a, int_of_string b)
      | _ -> failwith "move_of_string")
  | s when s.[0] = 'p' -> Partner (s.[1], s.[3])
  | _                  -> failwith "move_of_string"

let dances initial moves =
  let length = String.length initial in
  let swap s a b =
    String.init length (fun i -> if i = a then s.[b] else if i = b then s.[a] else s.[i]) in
  let swap_chars s c d =
    String.init length (fun i -> if s.[i] = c then d else if s.[i] = d then c else s.[i]) in
  let move s = function
      Spin x           -> (String.suffix s x) ^ (String.prefix s (length - x))
    | Exchange (a, b)  -> swap s a b
    | Partner (c, d)   -> swap_chars s c d
  in
  let moves = String.split moves ',' |> List.map ~f:move_of_string in
  let performances = 1000000000 in
  let rec dances s0 n =
    if n = performances then s0
    else
      let s1 = List.fold moves ~init:s0 ~f:move in
      if s1 = initial then dances s1 (performances - performances mod (succ n))
      else dances s1 (succ n)
  in
  dances initial 0

let _ =
  if not !Sys.interactive then
    match In_channel.input_line In_channel.stdin with
      Some moves -> dances "abcdefghijklmnop" moves
                    |> print_endline
    | None -> failwith "input"
