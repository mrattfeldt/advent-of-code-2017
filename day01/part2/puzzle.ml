(* Day 1: Inverse Captcha *)

(* Build with 'ocamlopt puzzle.ml -o puzzle' *)
(* Run tests with './puzzle test' *)
(* Run with './puzzle < <input-file>' *)

let inverse_captcha digits =
  let len = String.length digits in
  let digits_arr = Array.init len (fun i -> Char.code (String.get digits (i mod len)) - 48) in
  Array.fold_left (+) 0 (Array.mapi (fun i d -> if d = digits_arr.((i + (len / 2)) mod len) then d else 0) digits_arr)

let tests =
  [("1212"     ,  6);
   ("1221"     ,  0);
   ("123425"   ,  4);
   ("123123"   , 12);
   ("12131415" ,  4)]

let _ =
  if Array.length Sys.argv > 1 &&  Sys.argv.(1) = "test" then
    List.iter (fun (digits, verification) ->
        let result = inverse_captcha digits in
        if result <> verification then
          print_endline (digits ^ " was " ^ (string_of_int result) ^ " but should be " ^ (string_of_int verification))) tests
  else
    print_endline (string_of_int (inverse_captcha (read_line ())))
