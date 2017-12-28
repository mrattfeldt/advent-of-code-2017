(* Day 24: Electromagnetic Moat *)

(* ocamlbuild -use-ocamlfind -pkgs core -tag thread solution.native *)
(* ./solution.native < <input-file> *)

open Core

type component = {p1 : int; p2 : int}

module TabuMatching = struct
  type t = component * component
  let sexp_of_t (c1, c2) = List.sexp_of_t Int.sexp_of_t [c1.p1; c1.p2; c2.p1; c2.p2]
  let t_of_sexp s = match List.t_of_sexp Int.t_of_sexp s with
      [c1p1; c1p2; c2p1; c2p2] -> ({p1=c1p1; p2=c1p2}, {p1=c2p1; p2=c2p2})
    | _ -> failwith "error"
  let compare = Pervasives.compare
end

module Tabu = Set.Make(TabuMatching)

let component_of_string component =
  match String.split component '/' with
    [port1; port2] -> {p1 = int_of_string port1; p2 = int_of_string port2}
  | _              -> failwith "component_of_string"

let strength = List.fold ~init:0 ~f:(fun a c -> a + c.p1 + c.p2)

let matching tabu current c =
  match current with
    d :: _ when tabu (c, d) -> false
  | [d]            -> c.p1 <> 0 && c.p2 <> 0
                      && (c.p1 = d.p1 || c.p1 = d.p2 || c.p2 = d.p1 || c.p2 = d.p2)
  | d1 :: d2 :: _  -> (c.p1 = d1.p1 || c.p2 = d1.p1) && d1.p1 = d1.p2
                      || (c.p1 = d1.p1 || c.p2 = d1.p1) && d1.p1 <> d2.p1 && d1.p1 <> d2.p2
                      || (c.p1 = d1.p2 || c.p2 = d1.p2) && d1.p2 <> d2.p1 && d1.p2 <> d2.p2
  | []             -> failwith "matching"

let strongest components =
  let max cs1 cs2 =
    let l1, l2 = List.length cs1, List.length cs2 in
    if l1 > l2 then cs1 else if l2 > l1 then cs2
    else let s1, s2 = strength cs1, strength cs2 in
         if s1 > s2 then cs1 else cs2
  in
  let rec loop tabu strongest current = function
      []    -> max strongest current
      | cs  ->
         match List.find cs (matching (Tabu.mem tabu) current) with
           None   -> max strongest current
         | Some c -> let cs1 = List.filter cs ((<>)c) in
                     max (loop tabu strongest (c :: current) cs1)
                         (loop (Tabu.add tabu (c, List.hd_exn current)) strongest current (cs1@[c]))
  in
  List.filter components (fun c -> c.p1 = 0 || c.p2 = 0)
  |> List.fold ~init:[] ~f:(fun strong start ->
                 loop Tabu.empty strong [start] (List.filter components ((<>)start)))
  |> strength

let _ =
  if not !Sys.interactive then
    In_channel.input_lines In_channel.stdin
    |> List.map ~f:component_of_string
    |> strongest
    |> string_of_int
    |> print_endline
