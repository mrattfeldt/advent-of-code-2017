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
  fold_left (+) 0
            (map (fun row ->
                 let max_val = fold_left max 0 row in
                 let min_val = fold_left min max_int row in
                 max_val - min_val) data)

let tests =
  [("5 1 9 5\n"
    ^ "7 5 3\n"
    ^ "2 4 6 8", 18)]

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
