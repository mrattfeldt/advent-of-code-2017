(* Day 25: The Halting Problem *)

(* ocamlbuild -use-ocamlfind -pkgs core -tag thread solution.native *)
(* ./solution.native < <input-file> *)

open Core

type bit = Zero | One

let int_of_bit = function Zero -> 0 | One -> 1
let bit_of_int = function 0 -> Zero | 1 -> One | _ -> failwith "bit_of_int"

type transition = bit * int * char

let direction_of_string = function
    "right." -> 1
  | "left."  -> -1
  | _        -> failwith "direction_of_string"

module Tape = Map.Make(Int)

let turing (start, steps, states) =
  let state_trans = List.Assoc.find_exn ~equal:(=) states in
  let rec loop step tape transition cursor =
    if step = steps then
      Tape.fold tape ~init:0 ~f:(fun ~key:_key ~data:data ack -> ack + int_of_bit data)
    else
      let current = Tape.find tape cursor |> Option.value ~default:Zero in
      let write, move, next = transition current in
      loop (succ step) (Tape.set tape cursor write) (state_trans next) (cursor + move)
  in
  loop 0 Tape.empty (state_trans start) 0

let read_blueprint lines =
  let scan s = Scanf.bscanf (Scanf.Scanning.from_string s) in
  let rec read_state = function
      [] -> []
    | l :: ls when l = "" -> read_state ls
    | state_name :: _zero :: w0 :: m0 :: c0 :: _one :: w1 :: m1 :: c1 :: ls ->
       let (zero_transition, one_transition) =
         (scan w0 "    - Write the value %d." bit_of_int,
          scan m0 "    - Move one slot to the %s" direction_of_string,
          scan c0 "    - Continue with state %c." Fn.id),
         (scan w1 "    - Write the value %d." bit_of_int,
          scan m1 "    - Move one slot to the %s" direction_of_string,
          scan c1 "    - Continue with state %c." Fn.id) in
       (scan state_name "In state %c:" Fn.id,
        function Zero -> zero_transition | One -> one_transition) :: read_state ls
    | _ -> failwith "read_state"
  in
    match lines with
      start :: steps :: _blank :: states ->
      (scan start "Begin in state %c." Fn.id,
       scan steps "Perform a diagnostic checksum after %d steps." Fn.id,
       read_state states)
    | _ -> failwith "read_blueprint"

let _ =
  if not !Sys.interactive then
    In_channel.input_lines In_channel.stdin
    |> read_blueprint
    |> turing
    |> string_of_int
    |> print_endline
