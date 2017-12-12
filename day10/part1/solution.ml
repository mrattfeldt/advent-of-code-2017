(* Day 10: Knot Hash *)

(* ocamlbuild -use-ocamlfind -pkgs core -tag thread solution.native *)
(* ./solution.native < <input-file> *)

open Core

type state = {circle: int list; pos: int; skip: int}

exception Error

let sublist pos len = List.filteri ~f:(fun i e -> pos <= i && i < pos + len)
                   
let product circle lengths =
  let n = List.length circle in
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
  let initial_state = {circle = circle; pos = 0; skip = 0} in
  match (List.fold lengths ~init:initial_state ~f:advance).circle with
    x :: y :: _ -> x * y
  | _           -> raise Error

let _ =
  if not !Sys.interactive then
    match In_channel.input_line In_channel.stdin with
      Some input  -> String.split input ','
                     |> List.map ~f:int_of_string
                     |> (product (List.init 256 (fun i -> i)))
                     |> string_of_int
                     |> print_endline
    | None        -> print_endline "error"

let tests = [([0;1;2;3;4], [3; 4; 1; 5], 12);
             ([0;1;2;3;4], [0], 0);
             ([0;1;2;3;4], [5], 12);
             ([0;1;2;3;4], [3; 4; 0; 1; 5], 0)]

let _ =
  if !Sys.interactive then
    let _ = print_endline "Running tests..." in
    let _ =
      List.iter tests (fun (circle, lengths, expected) ->
                  let product = product circle lengths in
                  print_endline (if product = expected then "PASSED" else "FAILED"))
    in
    print_endline "Running tests...DONE"
