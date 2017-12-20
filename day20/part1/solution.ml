(* Day 20: Particle Swarm *)

(* ocamlbuild -use-ocamlfind -pkgs core -tag thread solution.native *)
(* ./solution.native < <input-file> *)

open Core

type xyz = {x: int; y: int; z: int}

let (+=) xyz incr = {x = xyz.x + incr.x; y = xyz.y + incr.y; z = xyz.z + incr.z}

type particle = {i: int; p: xyz; v: xyz; a: xyz}

let rec simulate count particles =
  let tick particle =
    let velocity = particle.v += particle.a in
    {particle with p = particle.p += velocity; v = velocity}
  in
  let distance particle = abs particle.p.x + abs particle.p.y + abs particle.p.z in
  if count = 0 then
    match List.min_elt particles ~cmp:(fun p1 p2 -> compare (distance p1) (distance p2)) with
      Some p -> p.i
    | None -> failwith "min_dist"
  else
    simulate (pred count) (List.map particles tick)

let particle_of_string index particle =
  Scanf.bscanf (Scanf.Scanning.from_string particle) "p=<%d,%d,%d>, v=<%d,%d,%d>, a=<%d,%d,%d>"
               (fun px py pz vx vy vz ax ay az ->
                 {i=index; p = {x=px; y=py; z=pz}; v = {x=vx; y=vy; z=vz}; a = {x=ax; y=ay; z=az}})

let _ =
  if not !Sys.interactive then
    In_channel.input_lines In_channel.stdin
    |> List.mapi ~f:particle_of_string
    |> simulate (10000)
    |> string_of_int
    |> print_endline
