(* Day 4: High-Entropy Passphrases *)

(* ocamlbuild -use-ocamlfind -pkgs core -tag thread solution.native *)
(* ./solution.native < <input-file> *)

open Core

let read_input () =
  let rec loop () =
    try
      let line = In_channel.(input_line_exn stdin) in
      line :: loop ()
    with End_of_file -> [] in
  loop ()

let valid passphrase =
  let anagram w1 w2 =
    let canonical1 = List.sort ~cmp:compare (String.to_list w1) in
    let canonical2 = List.sort ~cmp:compare (String.to_list w2) in
    compare canonical1 canonical2
  in
  not (List.contains_dup ~compare:anagram (String.split passphrase ' '))

let _ =
  if not !Sys.interactive then
    let passphrases = read_input () in
    print_endline (string_of_int (List.count passphrases valid))

(* Test code below *)

let tests = [("abcde fghij", true);
             ("abcde xyz ecdab", false);
             ("a ab abc abd abf abj", true);
             ("iiii oiii ooii oooi oooo", true);
             ("oiii ioii iioi iiio", false)]

let _ =
  if !Sys.interactive then
    let _ = print_endline "Running tests..." in
    let _ = List.iter tests (fun (instance, expected) ->
                        print_endline (if valid instance = expected then "PASSED" else "FAILED")) in
    print_endline "Running tests...DONE"
