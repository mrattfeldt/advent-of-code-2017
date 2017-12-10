(* Day 7: Recursive Circus *)

(* ocamlbuild -use-ocamlfind -pkgs core,str -tag thread solution.native *)
(* ./solution.native < <input-file> *)

open Core

module StringMap = Map.Make(String)

exception Error

let bottom data =
  let holds prog = StringMap.find_exn data prog |> snd in  
  let all_progs  = StringMap.keys data in
  let is_bottom prog =
    holds prog <> []
    && (List.for_all all_progs
                     ~f:(fun another_prog ->
                       not (List.mem (holds another_prog) ~equal:(=) prog)))
  in
  match List.filter all_progs ~f:is_bottom with
    [prog] -> prog
  | _      -> raise Error

let difference data =
  let weight prog = StringMap.find_exn data prog |> fst in
  let holds prog  = StringMap.find_exn data prog |> snd in
  let rec weights prog = List.fold (holds prog) ~init:(weight prog) ~f:(fun ack p -> ack + weights p) in
  let rec unbalanced prog =
    let prog_holds_weights = holds prog |> List.map ~f:(fun p -> (p, weights p)) in
    let first_weight = List.hd_exn prog_holds_weights |> snd in
    let (as_first_weight, not_as_first_weight) =
      List.partition_tf prog_holds_weights (fun (p, w) -> w = first_weight) in
    if not_as_first_weight = [] then
      None
    else
      match (as_first_weight, not_as_first_weight) with
        ([(unbalanced_prog, unbalanced_weight)], (balanced_prog, balanced_weight)::_::_)
      | ((balanced_prog, balanced_weight)::_::_, [(unbalanced_prog, unbalanced_weight)]) ->
         begin
           match unbalanced unbalanced_prog with
             None ->
             let correct_weight = weight unbalanced_prog + (balanced_weight - unbalanced_weight) in
             Some (unbalanced_prog, correct_weight)
           | other -> other
         end
      | _ -> raise Error
  in
  unbalanced (bottom data)

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
    let data = extract_data (In_channel.input_lines In_channel.stdin) in
    match difference data with
      Some (prog, diff) -> print_endline (string_of_int diff)
    | None -> raise Error

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
              Some ("ugml", 60))]

let _ =
  if !Sys.interactive then
    let _ = print_endline "Running tests..." in
    let _ =
      List.iter tests (fun (data, expected) ->
                  print_endline (if difference data = expected then "PASSED" else "FAILED"))
    in
    print_endline "Running tests...DONE"
