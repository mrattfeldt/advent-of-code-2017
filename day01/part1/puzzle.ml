(* Day 1: Inverse Captcha *)

(* Build with 'ocamlbuild puzzle.native' *)
(* Run tests with './puzzle.native test' *)
(* Run with './puzzle.native < <input-file>' *)

let inverse_captcha digits =
  let len = String.length digits in
  let digits_arr = Array.init len (fun i -> Char.code (String.get digits (i mod len)) - 48) in
  Array.fold_left (+) 0 (Array.mapi (fun i d -> if d = digits_arr.((succ i) mod len) then d else 0)
                                    digits_arr)

let tests =
  [("1122"     , 3);
   ("1111"     , 4);
   ("1234"     , 0);
   ("91212129" , 9)]

let _ =
  if Array.length Sys.argv > 1 &&  Sys.argv.(1) = "test" then
    List.iter (fun (digits, verification) ->
        let result = inverse_captcha digits in
        if result <> verification then
          print_endline (digits ^ " was " ^ (string_of_int result)
                         ^ " but should be " ^ (string_of_int verification))) tests
  else
    print_endline (string_of_int (inverse_captcha (read_line ())))
