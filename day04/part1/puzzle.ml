(* Day 4: High-Entropy Passphrases *)

(* ocamlbuild -use-ocamlfind -pkgs core -tag thread puzzle.native *)
(* ./puzzle.native < <input-file> *)

open Core

let read_input () =
  let rec loop () =
    try
      let line = In_channel.(input_line_exn stdin) in
      line :: loop ()
    with End_of_file -> [] in
  loop ()

let valid passphrase = not (List.contains_dup (String.split passphrase ' '))

let _ =
  if not !Sys.interactive then
    let passphrases = read_input () in
    print_endline (string_of_int (List.count passphrases valid))

let tests = [("aa bb cc dd ee", true);
             ("aa bb cc dd aa", false);
             ("aa bb cc dd aaa", true)]

let _ =
  if !Sys.interactive then
    let _ = print_endline "Running tests..." in
    let _ = List.iter tests (fun (instance, expected) ->
                        print_endline (if valid instance = expected then "PASSED" else "FAILED")) in
    print_endline "Running tests...DONE"
