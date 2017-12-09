(* Day 7: Recursive Circus *)

(* ocamlbuild -use-ocamlfind -pkgs core,str -tag thread solution.native *)
(* ./solution.native < <input-file> *)

open Core

module StringMap = Map.Make(String)

exception Error

let bottom data =
  let holds prog = StringMap.find_exn data prog |> snd in  
  let all_progs = StringMap.keys data in
  let is_bottom prog =
    holds prog <> []
    && (List.for_all all_progs
                     ~f:(fun another_prog ->
                       not (List.mem (holds another_prog) ~equal:(=) prog)))
  in
  match List.filter all_progs ~f:is_bottom with
    [prog] -> prog
  | _      -> raise Error

let extract_data lines =
  let extract_line line =
    match Str.split (Str.regexp "[ \t]+") line with
      prog :: weight :: arrow_holds ->
      let prog_holds =
        match arrow_holds with
          _arrow :: holds -> List.map holds (String.strip ~drop:((=)','))
        | _               -> []
      in
      (prog, (String.strip ~drop:(fun c -> c = '(' || c = ')') weight |> int_of_string, prog_holds))
    | _ -> raise Error
  in
  StringMap.of_alist_exn (List.map lines extract_line)

let _ =
  if not !Sys.interactive then
    print_endline (bottom (extract_data (In_channel.input_lines In_channel.stdin)))

(* Test code below *)

let tests = [(StringMap.of_alist_exn [("pbga", (66, []));
                                      ("xhth", (57, []));
                                      ("ebii", (61, []));
                                      ("havc", (66, []));
                                      ("ktlj", (57, []));
                                      ("fwft", (72, ["ktlj"; "cntj"; "xhth"]));
                                      ("qoyq", (66, []));
                                      ("padx", (45, ["pbga"; "havc"; "qoyq"]));
                                      ("tknk", (41, ["ugml"; "padx"; "fwft"]));
                                      ("jptl", (61, []));
                                      ("ugml", (68, ["gyxo"; "ebii"; "jptl"]));
                                      ("gyxo", (61, []));
                                      ("cntj", (57, []))],
              "tknk")]

let _ =
  if !Sys.interactive then
    let _ = print_endline "Running tests..." in
    let _ =
      List.iter tests (fun (data, expected) ->
                  print_endline (if bottom data = expected then "PASSED" else "FAILED")) in
    print_endline "Running tests...DONE"
