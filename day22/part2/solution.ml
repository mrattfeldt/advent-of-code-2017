(* Day 22: Sportifica Virus *)

(* ocamlbuild -use-ocamlfind -pkgs batteries solution.native *)
(* ./solution.native < <input-file> *)

open Batteries

module Coord = struct
  type t = int * int
  let compare = Pervasives.compare
end

module Grid = Map.Make(Coord)

type direction = Down | Right | Up | Left

type virus = {pos: Coord.t; dir: direction}

type node = Clean | Weak | Infect | Flag

let turn_left v =
  {v with dir = match v.dir with Down -> Right | Right -> Up | Up -> Left | Left -> Down}

let turn_right v =
  {v with dir = match v.dir with Down -> Left | Right -> Down | Up -> Right | Left -> Up}

let reverse v =
  {v with dir = match v.dir with Down -> Up | Right -> Left | Up -> Down | Left -> Right}

let move_forward v =
  let (x, y) = v.pos in
  {v with pos = match v.dir with Down -> (x,y+1) | Right -> (x+1,y) | Up -> (x,y-1) | Left -> (x-1,y)}

type state = {virus: virus; grid: node Grid.t; count: int}

let rec do_bursts nbursts state =
  let do_burst state =
    let (grid, virus, pos) = (state.grid, state.virus, state.virus.pos) in
    let (virus1, grid1, count1) =
      match Grid.find_default Clean pos grid with
        Clean  -> (turn_left virus, Grid.add pos Weak grid, state.count)
      | Weak   -> (virus, Grid.add pos Infect grid, succ state.count)
      | Infect -> (turn_right virus, Grid.add pos Flag grid, state.count)
      | Flag   -> (reverse virus, Grid.remove pos grid, state.count)
    in
    {virus = virus1 |> move_forward; grid = grid1; count = count1}
  in
  if nbursts = 0 then state.count
  else do_bursts (pred nbursts) (do_burst state)

let bursts nbursts infected =
  let size = List.length infected in
  let virus = {pos = (size / 2, size / 2); dir = Up} in
  let grid = List.concat infected 
             |> List.fold_left (fun ack p -> Grid.add p Infect ack) Grid.empty in
  do_bursts nbursts {virus = virus; grid = grid; count = 0}
  
let row_of_string y row =
  String.to_list row
  |> List.mapi (fun x c -> if c = '#' then [(x, y)] else [])
  |> List.concat
                      
let _ =
  if not !Sys.interactive then
    let rec read_lines () =
      try let line = IO.read_line IO.stdin in
          line :: read_lines ()
      with IO.No_more_input -> []
    in
    let nbursts = int_of_string Sys.argv.(1) in
    read_lines ()
    |> List.mapi row_of_string
    |> bursts nbursts
    |> string_of_int
    |> print_endline
