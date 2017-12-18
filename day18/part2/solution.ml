(* Day 18: Permutation Duet *)

(* ocamlbuild -use-ocamlfind -pkgs core -tag thread solution.native *)
(* ./solution.native < <input-file> *)

open Core
open Option.Monad_infix

module R = Map.Make(String)

type arg = Reg of string | Value of int

let arg_of_string argstr =
  if Char.is_alpha argstr.[0] then Reg argstr
  else Value (int_of_string argstr)

type instr = Snd of arg
           | Rcv of string
           | Set of string * arg
           | Add of string * arg 
           | Mul of string * arg
           | Mod of string * arg
           | Jgz of arg * arg

let instr_of_string instr =
  match String.split instr ' ' with
    "snd" :: arg :: _           -> Snd (arg_of_string arg)
  | "set" :: reg :: arg ::_     -> Set (reg, arg_of_string arg)
  | "add" :: reg :: arg ::_     -> Add (reg, arg_of_string arg)
  | "mul" :: reg :: arg ::_     -> Mul (reg, arg_of_string arg)
  | "mod" :: reg :: arg ::_     -> Mod (reg, arg_of_string arg)
  | "rcv" :: reg ::_            -> Rcv reg
  | "jgz" :: arg1 :: arg2 :: _  -> Jgz (arg_of_string arg1, arg_of_string arg2)
  | _                           -> failwith "instr_of_string"

type state = {count: int; messages: (int * int) list; registers: int R.t}

let rec list_find_drop messages p = 
  List.findi messages (fun _ (q, _) -> q = p)
  >>| fun (index, (_, m)) -> m, List.filteri messages (fun i _ -> i <> index)

let run instrs =
  let instr_reg = "0" in
  let instr_reg_of_prog p = string_of_int p ^ instr_reg in
  let fetch p registers r = R.find registers (string_of_int p ^ r) |> Option.value ~default:0 in
  let deref p registers = function
      Reg r   -> fetch p registers r
    | Value v -> v
  in
  let execute p state instr =
    let otherp      = succ p mod 2 in
    let fetch       = fetch p state.registers in
    let deref       = deref p state.registers in
    let update a b  = R.add state.registers (string_of_int p ^ a) b in
    let incr_instr registers = R.add registers (instr_reg_of_prog p) (succ (fetch instr_reg)) in
    match instr with
      Snd arg          -> Some {messages  = state.messages @ [otherp, deref arg];
                                registers = incr_instr state.registers;
                                count     = state.count + p}
    | Rcv reg          -> list_find_drop state.messages p
                          >>| fun (m, messages1) ->
                          {state with messages  = messages1;
                                      registers = update reg m                          |> incr_instr}
    | Set (reg, arg)   -> Some {state with registers = update reg (deref arg)           |> incr_instr}
    | Add (reg, arg)   -> Some {state with registers = update reg (fetch reg + deref arg)
                                                       |> incr_instr}
    | Mul (reg, arg)   -> Some {state with registers = update reg (fetch reg  * deref arg)
                                                       |> incr_instr}
    | Mod (reg, arg)   -> Some {state with registers = update reg (fetch reg  mod deref arg)
                                                       |> incr_instr}
    | Jgz (arg1, arg2) -> Some {state with registers =
                                             if deref arg1 > 0 then
                                               update instr_reg (fetch instr_reg + deref arg2)
                                             else
                                               incr_instr state.registers}
  in
  let rec run state =
    match
      List.find_map [0; 1] (fun p -> execute p state (instrs.(fetch p state.registers instr_reg)))
    with
      None        -> state.count
    | Some state1 -> run state1
  in
  run {count = 0; messages = []; registers = R.of_alist_exn [("0p", 0); ("1p", 1)]}

let _ =
  if not !Sys.interactive then
    In_channel.input_lines In_channel.stdin
    |> List.map ~f:instr_of_string
    |> Array.of_list
    |> run
    |> string_of_int
    |> print_endline
