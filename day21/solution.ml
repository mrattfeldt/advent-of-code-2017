(* Day 21: Fractal Art *)

(* ocamlbuild -use-ocamlfind -pkgs core -tag thread solution.native *)
(* ./solution.native <iterations> < <input-file> *)

open Core

let ( >> ) = Fn.compose

type image = string

type rule = {input: image; output: image}

let image_of_string image = List.fold ~init:"" ~f:(^) (String.split image '/')

let rule_of_string rule =
  Scanf.bscanf (Scanf.Scanning.from_string rule) "%s => %s"
               (fun input output -> {input = image_of_string input; output = image_of_string output})

let int_sqrt x = sqrt (float_of_int x ) |> int_of_float 

let image_size image = int_sqrt (String.length image)

let coi size index  = (index mod size, index / size)
let ioc size (x, y) = y * size + x

let (fliph, flipv, rot90) =
  let trans trans_index image =
    let size = image_size image in
    String.init (size*size) (fun index -> image.[(trans_index size index)])
  in
  (trans (fun size index -> let (x, y) = coi size index in (x, size - 1 - y) |> ioc size),
   trans (fun size index -> let (x, y) = coi size index in (size - 1 - x, y) |> ioc size),
   trans (fun size index -> let (x, y) = coi size index in (y, size - 1 - x) |> ioc size))

let transforms = [Fn.id; fliph; flipv; rot90; rot90 >> rot90;
                  rot90 >> rot90 >> rot90; rot90 >> fliph; rot90 >> flipv]

let enhance_square rules square =
  let size = image_size square in
  let match_transform rule transform = Option.some_if (transform rule.input = square) rule.output in
  let enhance rule =
    if size <> image_size rule.input then None
    else List.find_map transforms (match_transform rule)
  in
  match List.find_map rules enhance with
      Some image1 -> image1
    | None        -> failwith "enhance_square"

let square_indexes image =
  let size = image_size image in
  let stride = if size mod 2 = 0 then 2 else 3 in
  let starts = List.range ~stride 0 size in
  let square_index (x, y) =
    if stride = 2 then [(x,y); (x+1,y); (x,y+1); (x+1,y+1)]
    else [(x,y); (x+1,y); (x+2,y); (x,y+1); (x+1,y+1); (x+2,y+1); (x,y+2); (x+1,y+2); (x+2,y+2)]
  in
  List.concat_map starts (fun y -> List.map starts (fun x -> (x, y)))
  |> List.map ~f:(fun start -> square_index start |> List.map ~f:(ioc size))

let arrange_squares square_size squares =
  let size = int_sqrt (List.fold ~init:0 ~f:(fun ack s -> ack + String.length s) squares) in
  let rec arrange squares =
    if List.for_all squares ((=)"") then ""
    else
      let row =
        List.fold ~init:""
                  ~f:(fun ack s ->
                    if String.length ack < size then ack ^ String.prefix s square_size else ack)
                  squares
      and squares1 =
        List.mapi ~f:(fun i s -> if i*square_size < size then String.drop_prefix s square_size else s)
                  squares
      in
      row ^ arrange (squares1 |> List.filter ~f:((<>)""))
  in
  arrange squares

let enhance_once rules image =
  let square_indexes = square_indexes image in
  List.map square_indexes (fun indexes -> List.map indexes (String.get image) |> String.of_char_list)
  |> List.map ~f:(enhance_square rules)
  |> arrange_squares (if image_size image mod 2 = 0 then 3 else 4)

let enhance iterations rules =
  let start_image = image_of_string ".#./..#/###" in
  let rec loop iterations image =
    if iterations = 0 then image
    else loop (pred iterations) (enhance_once rules image)
  in
  loop iterations start_image
  |> String.count ~f:((=)'#')

let _ =
  if not !Sys.interactive then
    let iterations = Sys.argv.(1) |> int_of_string in
    In_channel.input_lines In_channel.stdin
    |> List.map ~f:rule_of_string
    |> enhance iterations
    |> string_of_int
    |> print_endline
