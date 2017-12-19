(* Day 19: A Series of Tubes *)

(* ocamlbuild -use-ocamlfind -pkgs core -tag thread solution.native *)
(* ./solution.native < <input-file> *)

open Core

module Coord = struct
  type t = int * int
  let sexp_of_t (x, y) = List.sexp_of_t Int.sexp_of_t [x; y]
  let t_of_sexp s = match List.t_of_sexp Int.t_of_sexp s with [x;y] -> (x, y) | _ -> failwith "error"
  let compare = Pervasives.compare
end

module Grid = Map.Make(Coord)

type direction = Down | Right | Up | Left

let straight (row, col) = function
    Down    -> ((row+1, col), Down)
  | Right   -> ((row, col+1), Right)
  | Up      -> ((row-1, col), Up)
  | Left    -> ((row, col-1), Left)

let left (row, col) = function
    Down    -> ((row, col+1), Right)
  | Right   -> ((row-1, col), Up)
  | Up      -> ((row, col-1), Left)
  | Left    -> ((row+1, col), Down)

let right (row, col) = function
    Down    -> ((row, col-1), Left)
  | Right   -> ((row+1, col), Down)
  | Up      -> ((row, col+1), Right)
  | Left    -> ((row-1, col), Up)

let trace cells =
  let grid = Grid.of_alist_exn cells in
  let next coord direction =
    List.find [straight coord direction; left coord direction; right coord direction]
              (fun (coord1, direction1) -> Grid.mem grid coord1)
  in
  let rec trace letters coord direction =
    let letters1 = match Grid.find grid coord with
        Some c when Char.is_alpha c  -> c :: letters
      | _                            -> letters
    in
    match next coord direction with
      None                      -> List.rev letters1 |> String.of_char_list
    | Some (coord1, direction1) -> trace letters1 coord1 direction1
  in
  let start, _ = List.find_exn cells ~f:(fun ((r, _), _) -> r = 0) in
  trace [] start Down

let line_to_cells row line =
  String.to_list line
  |> List.concat_mapi ~f:(fun col cell -> if cell <> ' ' then [((row, col), cell)] else [])

let _ =
  if not !Sys.interactive then
    In_channel.input_lines In_channel.stdin
    |> List.concat_mapi ~f:line_to_cells
    |> trace
    |> print_endline
