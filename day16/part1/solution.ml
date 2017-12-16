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

let dance initial moves =
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
  String.split moves ','
  |> List.map ~f:move_of_string
  |> List.fold ~init:initial ~f:move

let _ =
  if not !Sys.interactive then
    match In_channel.input_line In_channel.stdin with
      Some moves -> dance "abcdefghijklmnop" moves
                    |> print_endline
    | None -> failwith "input"

(* Test code below *)

let tests = [("abcde", "s1,x3/4,pe/b", "baedc")]

let _ =
  if !Sys.interactive then
    let _ = print_endline "Running tests..." in
    let _ =
      List.iter tests (fun (initial, moves, expected) ->
                  let result = dance initial moves in
                  print_endline (if result = expected then "PASSED" else "FAILED")) in
    print_endline "Running tests...DONE"
