(* Day 22: Sportifica Virus *)

(* ocamlbuild -use-ocamlfind -pkgs core -tag thread solution.native *)
(* ./solution.native < <input-file> *)

open Core

module Coord = struct
  type t = int * int
  let sexp_of_t (x, y) = List.sexp_of_t Int.sexp_of_t [x; y]
  let t_of_sexp s = match List.t_of_sexp Int.t_of_sexp s with [x;y] -> (x, y) | _ -> failwith "error"
  let compare = Pervasives.compare
end

module Grid = Set.Make(Coord)

type direction = Down | Right | Up | Left

type virus = {pos: Coord.t; dir: direction}

let turn_left v =
  {v with dir = match v.dir with Down -> Right | Right -> Up | Up -> Left | Left -> Down}

let turn_right v =
  {v with dir = match v.dir with Down -> Left | Right -> Down | Up -> Right | Left -> Up}

let move_forward v =
  let (x, y) = v.pos in
  {v with pos = match v.dir with Down -> (x,y+1) | Right -> (x+1,y) | Up -> (x,y-1) | Left -> (x-1,y)}

type state = {virus: virus; grid: Grid.t; count: int}

let rec do_bursts nbursts state =
  let do_burst state =
    let (virus1, grid1, count1) =
      match Grid.mem state.grid state.virus.pos with
        true  -> (turn_right state.virus, Grid.remove state.grid state.virus.pos, state.count)
      | false -> (turn_left state.virus, Grid.add state.grid state.virus.pos, succ state.count)
    in
    {virus = (virus1 |> move_forward); grid = grid1; count = count1}
  in
  if nbursts = 1 then state.count
  else do_bursts (pred nbursts) (do_burst state)

let bursts nbursts infected =
  let size = List.length infected in
  let virus = {pos = (size / 2, size / 2); dir = Up} in
  let grid = List.concat infected |> Grid.of_list in
  do_bursts nbursts {virus = virus; grid = grid; count = 0}
  
let row_of_string y row =
  String.to_list row
  |> List.concat_mapi ~f:(fun x c -> if c = '#' then [(x, y)] else [])
                      
let _ =
  if not !Sys.interactive then
    let nbursts = int_of_string Sys.argv.(1) in
    In_channel.input_lines In_channel.stdin
    |> List.mapi ~f:row_of_string
    |> bursts nbursts
    |> string_of_int
    |> print_endline
