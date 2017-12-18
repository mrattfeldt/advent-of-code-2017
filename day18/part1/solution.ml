(* Day 18: Permutation Duet *)

(* ocamlbuild -use-ocamlfind -pkgs core -tag thread solution.native *)
(* ./solution.native < <input-file> *)

open Core

module R = Map.Make(Char)

type arg = Reg of char | Value of int

type instr = Snd of arg
           | Set of char * arg
           | Add of char * arg
           | Mul of char * arg
           | Mod of char * arg
           | Rcv of arg
           | Jgz of arg * arg

let arg_of_string argstr =
  if Char.is_alpha argstr.[0] then Reg argstr.[0]
  else Value (int_of_string argstr)

let instr_of_string instr =
  match String.split instr ' ' with
    "snd" :: arg :: _           -> Snd (arg_of_string arg)
  | "set" :: reg :: arg ::_     -> Set (reg.[0], arg_of_string arg)
  | "add" :: reg :: arg ::_     -> Add (reg.[0], arg_of_string arg)
  | "mul" :: reg :: arg ::_     -> Mul (reg.[0], arg_of_string arg)
  | "mod" :: reg :: arg ::_     -> Mod (reg.[0], arg_of_string arg)
  | "rcv" :: arg :: _           -> Rcv (arg_of_string arg)
  | "jgz" :: arg1 :: arg2 :: _  -> Jgz (arg_of_string arg1, arg_of_string arg2)
  | _                           -> failwith "instr_of_string"

let run instrs =
  let freq_reg  = '0' in
  let instr_reg = '1' in
  let fetch registers r = R.find registers r |> Option.value ~default:0 in
  let deref registers = function
      Reg r   -> fetch registers r
    | Value v -> v
  in
  let incr_instr registers = R.add registers instr_reg (succ (fetch registers instr_reg)) in
  let execute registers instr =
    let fetch = fetch registers in
    let deref = deref registers in
    let update a b = R.add registers a b in
    match instr with
      Snd arg          -> update freq_reg (deref arg)                                     |> incr_instr
    | Set (reg, arg)   -> update reg (deref arg)                                          |> incr_instr
    | Add (reg, arg)   -> update reg (fetch reg + deref arg)                              |> incr_instr
    | Mul (reg, arg)   -> update reg (fetch reg * deref arg)                              |> incr_instr
    | Mod (reg, arg)   -> update reg (fetch reg mod deref arg)                            |> incr_instr
    | Rcv arg          -> if deref arg <> 0 then failwith "execute" else registers        |> incr_instr
    | Jgz (arg1, arg2) -> if deref arg1 > 0 then update instr_reg (fetch instr_reg + deref arg2)
                          else registers |> incr_instr
  in
  let rec run registers =
    match instrs.(fetch registers instr_reg) with
      Rcv arg when deref registers arg <> 0 -> fetch registers freq_reg
    | instr                                 -> run (execute registers instr)
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
