(* Day 6: Memory Reallocation *)

(* ocamlbuild -use-ocamlfind -pkgs core,str -tag thread solution.native *)
(* ./solution.native < <input-file> *)

open Core

let detect banks =
  let length = Array.length banks in
  let rec loop count seen =
    let banks_list = Array.to_list banks in
    if List.mem seen banks_list (=) then
      count
    else
      let max_blocks = Option.value_exn (Array.max_elt banks compare) in
      let max_blocks_index = Array.findi_exn banks (fun _ blocks -> blocks = max_blocks) |> fst in
      let _update = banks.(max_blocks_index) <- 0 in
      let offsets = List.init max_blocks succ in
      let _ = List.iter offsets
                        (fun offset ->
                          let offset_index = (max_blocks_index + offset) mod length in
                          banks.(offset_index) <- succ banks.(offset_index)) in
      loop (succ count) (banks_list :: seen)
  in
  loop 0 []

let _ =
  if not !Sys.interactive then
    let banks =
      In_channel.input_lines In_channel.stdin
      |> List.hd_exn
      |> Str.split (Str.regexp "[ \t]+")
      |> List.map ~f:int_of_string
      |> Array.of_list in
    print_endline (string_of_int (detect banks))

(* Test code below *)

let tests = [([|0; 2; 7; 0|], 5)]

let _ =
  if !Sys.interactive then
    let _ = print_endline "Running tests..." in
    let _ = List.iter tests (fun (instance, expected) ->
                        print_endline (if detect instance = expected then "PASSED" else "FAILED")) in
    print_endline "Running tests...DONE"
