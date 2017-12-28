(* Day 23: Coprocessor Conflagration *)

(* ocamlbuild -use-ocamlfind -pkgs core -tag thread solution.native *)
(* ./solution.native < <input-file> *)

open Core

module R = Map.Make(Char)

type arg = Reg of char | Value of int

type instr = Set of char * arg
           | Sub of char * arg
           | Mul of char * arg
           | Jnz of arg * arg

let arg_of_string argstr =
  if Char.is_alpha argstr.[0] then Reg argstr.[0]
  else Value (int_of_string argstr)

let instr_of_string instr =
  match String.split instr ' ' with
    "set" :: reg :: arg ::_     -> Set (reg.[0], arg_of_string arg)
  | "sub" :: reg :: arg ::_     -> Sub (reg.[0], arg_of_string arg)
  | "mul" :: reg :: arg ::_     -> Mul (reg.[0], arg_of_string arg)
  | "jnz" :: arg1 :: arg2 :: _  -> Jnz (arg_of_string arg1, arg_of_string arg2)
  | _                           -> failwith "instr_of_string"

let run instrs =
  let mul_cnt_reg = '0' in
  let instr_reg   = '1' in
  let fetch registers r = R.find registers r |> Option.value ~default:0 in
  let deref registers = function
      Reg r   -> fetch registers r
    | Value v -> v
  in
  let incr_instr registers = R.set registers instr_reg (succ (fetch registers instr_reg)) in
  let execute registers instr =
    let fetch = fetch registers in
    let deref = deref registers in
    let update a b = R.set registers a b in
    match instr with
      Set (reg, arg)   -> update reg (deref arg)                                          |> incr_instr
    | Sub (reg, arg)   -> update reg (fetch reg - deref arg)                              |> incr_instr
    | Mul (reg, arg)   -> update reg (fetch reg * deref arg)
                          |> R.set ~key:mul_cnt_reg ~data:(succ (fetch mul_cnt_reg))      |> incr_instr
    | Jnz (arg1, arg2) -> if deref arg1 <> 0 then update instr_reg (fetch instr_reg + deref arg2)
                          else registers |> incr_instr
  in
  let rec run registers =
    let instr_index = fetch registers instr_reg in
    if 0 <= instr_index && instr_index < Array.length instrs then
      run (execute registers instrs.(fetch registers instr_reg))
    else
      fetch registers mul_cnt_reg
  in
  run R.empty

let _ =
  if not !Sys.interactive then
    In_channel.input_lines In_channel.stdin
    |> List.map ~f:instr_of_string
    |> Array.of_list
    |> run
    |> string_of_int
    |> print_endline
