(* Day 2: Corruption Checksum *)

(* Build with 'ocamlbuild -use-ocamlfind -pkgs str puzzle.native' *)
(* Run tests with './puzzle.native test' *)
(* Run with './puzzle.native < <input-file>' *)

open List

let read_input () =
  let rec loop () =
    try
      let line = read_line () in
      line :: loop ()
    with End_of_file -> [] in
  loop ()

let lines_to_data = map (fun line -> map int_of_string (Str.split (Str.regexp "[ \t]+") line))

let checksum data =
  let divisor row =
    let ordered = sort compare row in
    hd (flatten (map (fun a ->
                     (fold_left (fun ack b -> if ack = [] && a <> b && b mod a = 0 then [b/a] else ack)
                                [] ordered)) ordered))
  in
  fold_left (+) 0 (map divisor data)

let tests =
  [("5 9 2 8\n"
    ^ "9 4 7 3\n"
    ^ "3 8 6 5", 9)]

let _ =
  if Array.length Sys.argv > 1 &&  Sys.argv.(1) = "test" then
    List.iter (fun (spread, verification) ->
        let data = lines_to_data (String.split_on_char '\n' spread) in
        let result = checksum data in
        if result <> verification then
          print_endline (spread ^ "\n\n was \n\n"
                         ^ (string_of_int result)
                         ^ " \n\nbut should be\n\n "
                         ^ (string_of_int verification))) tests
  else
    let data = lines_to_data (read_input ()) in
    print_endline (string_of_int (checksum data))
