(* Day 8: I Heard You Like Registers *)

(* ocamlbuild -use-ocamlfind -pkgs core,str -tag thread solution.native *)
(* ./solution.native < <input-file> *)

open Core

module StringMap = Map.Make(String)

exception Error

let line_stream_of_string string = Stream.of_list (Str.split (Str.regexp "\n") string)

let rec stream_to_lines stream =
  try
    let next_line = Stream.next stream in
    next_line :: stream_to_lines stream
  with Stream.Failure -> []

let op_of_string = function
    "inc" -> (+)
  | "dec" -> (-)
  | _     -> raise Error

let rel_of_string = function
    "<"  -> (<)
  | "<=" -> (<=)
  | "==" -> (=)
  | "!=" -> (<>)
  | ">=" -> (>=)
  | ">"  -> (>)
  | _    -> raise Error

type instr = {reg: string; update: int -> int; creg: string; cond: int -> bool}

let extract_instructions input =
  let extract_line line =
    match Str.split (Str.regexp "[ \t]+") line with
      [reg; op; amount; _if; creg; rel; cmp] ->
      {reg  = reg;  update = (fun v -> (op_of_string op) v (int_of_string amount));
       creg = creg; cond   = (fun r -> (rel_of_string rel) r (cmp |> int_of_string))}
    | _ -> raise Error
  in
  (stream_to_lines input) |> List.map ~f:extract_line

let process_and_extract_all_max instructions =
  let process (registers, all_max) instr =
    let creg = StringMap.find registers instr.creg |> Option.value ~default:0 in
    match instr.cond creg with
      true  -> let updated_reg =
                 instr.update (StringMap.find registers instr.reg |> Option.value ~default:0) in
               (StringMap.add registers instr.reg updated_reg, max all_max updated_reg)
    | false -> (registers, all_max)
  in
  let (_processed, all_max) = List.fold instructions ~init:(StringMap.empty, 0) ~f:process in
  all_max
                            
let _ =
  if not !Sys.interactive then
    let instructions =
      extract_instructions (In_channel.input_all In_channel.stdin |> line_stream_of_string) in
    let all_max = process_and_extract_all_max instructions in
    print_endline (string_of_int all_max)

(* Test code below *)
let tests = [(line_stream_of_string ("b inc 5 if a > 1\n"
                                     ^ "a inc 1 if b < 5\n"
                                     ^ "c dec -10 if a >= 1\n"
                                     ^ "c inc -20 if c == 10\n"),
              10)]

let _ =
  if !Sys.interactive then
    let _ = print_endline "Running tests..." in
    let _ =
      List.iter tests (fun (input, expected) ->
                  let instructions = extract_instructions input in
                  let all_max    = process_and_extract_all_max instructions in
                  print_endline (if all_max = expected then "PASSED" else "FAILED")) in
    print_endline "Running tests...DONE"
