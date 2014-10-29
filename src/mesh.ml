open Gg
open Bigarray

type mesh =
  {
    mutable nb_vertices: int;
    mutable nb_half_edges: int;
    mutable vertices: v4 array;
    mutable half_edges: (int, int_elt, c_layout) Array2.t; 
  }

let show_mesh {half_edges; nb_half_edges; vertices; nb_vertices; _} =
  print_endline (Printf.sprintf "nb_vertices = %i; nb_half_edges = %i" nb_vertices nb_half_edges);
  for i = 0 to nb_vertices - 1 do
    V4.pp Format.std_formatter vertices.(i);
    Format.print_newline ()
  done;
  for i = 0 to nb_half_edges - 1 do
    print_endline 
      (Printf.sprintf "%3i: %3i %3i %3i %3i %3i %3i"
         i
         half_edges.{i, 0}
         half_edges.{i, 1}
         half_edges.{i, 2}
         half_edges.{i, 3}
         half_edges.{i, 4}
         half_edges.{i, 5}
      )
  done

let new_edge_id, reset_edge_ids =
  let i = ref (-1) in
  (fun () -> incr i; !i),
  (fun () -> i := (-1))

let start_v {half_edges; _} id = half_edges.{id, 0}
let end_v {half_edges; _} id = half_edges.{id, 1}
let face {half_edges; _} id = half_edges.{id, 2}
let prev {half_edges; _} id = half_edges.{id, 3}
let next {half_edges; _} id = half_edges.{id, 4}
let opposit {half_edges; _} id = half_edges.{id, 5}

let set_start_v {half_edges; _} id x = half_edges.{id, 0} <- x
let set_end_v {half_edges; _} id x = half_edges.{id, 1} <- x
let set_face {half_edges; _} id x = half_edges.{id, 2} <- x
let set_prev {half_edges; _} id x = half_edges.{id, 3} <- x
let set_next {half_edges; _} id x = half_edges.{id, 4} <- x
let set_opposit {half_edges; _} id x = half_edges.{id, 5} <- x

let add_face table face =
  let l = Array.length face in
  if l < 3 then
    invalid_arg "new_mesh: degenerated face."
  else
    let reverse = ref false in
    for i = 0 to l - 1 do
      reverse := if !reverse then true else Hashtbl.mem table (face.(i), face.((i + 1) mod l))
    done;
    if !reverse then begin
      for i = 0 to (l - 1) / 2 do
        let tmp = face.(i) in
        face.(i) <- face.(l - 1 - i);
        face.(l - 1 - i) <- tmp
      done
    end;
    let face_edge = face.(0), face.(1) in
    for i = 0 to l - 1 do
      let next = (i + 1) mod l in
      let next_next = (i + 2) mod l in
      let prev = (l + i - 1) mod l in
      let prev_edge = face.(prev), face.(i) in
      let next_edge = face.(next), face.(next_next) in
      let opposit_edge = face.(next), face.(i) in
      let id = new_edge_id () in
      Hashtbl.add table (face.(i), face.(next)) (id, face_edge, prev_edge, next_edge, opposit_edge)
    done

let get_id edge table =
  match Hashtbl.find table edge with
  | (id, _, _, _, _) -> id
  | exception Not_found -> (-1)

let rec fill_half_edges (start_v, end_v as edge) table array =
  match Hashtbl.find table edge with
  | (id, face, prev, next, opposit) ->
    let face_id = get_id face table in
    let prev_id = get_id prev table in
    let next_id = get_id next table in
    let opposit_id = get_id opposit table in
    array.{id, 0} <- start_v;
    array.{id, 1} <- end_v;
    array.{id, 2} <- face_id;
    array.{id, 3} <- prev_id;
    array.{id, 4} <- next_id;
    array.{id, 5} <- opposit_id;
    recurse next next_id table array;
    recurse prev prev_id table array;
    recurse opposit opposit_id table array
  | exception Not_found -> ()
  
and recurse edge id table array =
  if id <> (-1) then
    if array.{id, 0} = (-1) then
      fill_half_edges edge table array

let new_mesh vertices faces =
  reset_edge_ids ();
  let half_edges_table = Hashtbl.create (Array.length vertices) in
  List.iter (add_face half_edges_table) faces;
  let nb_half_edges = Hashtbl.length half_edges_table in
  let half_edges = Array2.create int c_layout nb_half_edges 6 in
  Array2.fill half_edges (-1);
  let first_edge =
    match faces with
    | [] -> invalid_arg "new_mesh: no faces."
    | face :: _ -> face.(0), face.(1)
  in
  fill_half_edges first_edge half_edges_table half_edges;
  {
    nb_vertices = Array.length vertices;
    nb_half_edges;
    vertices = Array.copy vertices;
    half_edges;
  }

let iter_edges f ({vertices; nb_half_edges; _} as mesh)=
  let flags = Array1.create int8_unsigned c_layout nb_half_edges in
  Array1.fill flags 0;
  let rec aux edge_id =
    flags.{edge_id} <- 1;
    let opposit_id = opposit mesh edge_id in
    if flags.{opposit_id} = 0 then f vertices.(start_v mesh edge_id) vertices.(end_v mesh edge_id);
    recurse (next mesh edge_id);
    recurse (prev mesh edge_id);
    recurse opposit_id
  and recurse edge_id =
    if flags.{edge_id} = 0 then aux edge_id
  in
  aux 0

let double_array_length nb array =
  let l = Array.length array in
  let result = Array.make (min (l * 2) Sys.max_array_length) V4.zero in
  Array.blit array 0 result 0 nb;
  result

let add_vertex ({nb_vertices; vertices; _} as mesh) v =
  let l = Array.length vertices in
  let vertices =
    if l <= nb_vertices then
      double_array_length nb_vertices vertices
    else
      vertices
  in
  vertices.(nb_vertices) <- v;
  mesh.nb_vertices <- nb_vertices + 1;
  mesh.vertices <- vertices;
  nb_vertices
  

let double_edges_length nb array =
  let l = Array2.dim1 array in
  let result = Array2.create int c_layout (l * 2) 6 in
  Array2.fill result (-1);
  Array2.blit array (Array2.sub_left result 0 l);
  result
    
let add_half_edge ({nb_half_edges; half_edges; _} as mesh) v1 v2 face prev next opposit =
  let l = Array2.dim1 half_edges in
  let half_edges =
    if l <= nb_half_edges then
      double_edges_length nb_half_edges half_edges
    else
      half_edges
  in
  half_edges.{nb_half_edges, 0} <- v1;
  half_edges.{nb_half_edges, 1} <- v2;
  half_edges.{nb_half_edges, 2} <- face;
  half_edges.{nb_half_edges, 3} <- prev;
  half_edges.{nb_half_edges, 4} <- next;
  half_edges.{nb_half_edges, 5} <- opposit;
  mesh.nb_half_edges <- nb_half_edges + 1;
  mesh.half_edges <- half_edges;
  nb_half_edges

let cut mesh edge_id v4 =
  let v = add_vertex mesh v4 in
  let opposit_ = opposit mesh edge_id in
  let face_ = face mesh edge_id in
  let op_face = face mesh opposit_ in
  let prev_ = prev mesh edge_id in
  let op_next = next mesh edge_id in
  let start_v_ = start_v mesh edge_id in
  let edge =
    add_half_edge mesh start_v_ v face_ prev_ edge_id (-1)
  in
  let opposit_edge =
    add_half_edge mesh v start_v_ op_face opposit_ op_next edge
  in
  set_opposit mesh edge opposit_edge;
  set_start_v mesh edge_id v;
  set_prev mesh edge_id edge;
  set_next mesh prev_ edge;
  set_end_v mesh opposit_ v;
  set_next mesh opposit_ opposit_edge;
  set_prev mesh op_next opposit_edge;
  edge

let connect_half mesh e1 e2 =
  let end1 = end_v mesh e1 in
  let start2 = start_v mesh e2 in
  let edge =
    add_half_edge mesh end1 start2 (-1) e1 e2 (-1)
  in
  set_next mesh e1 edge;
  set_prev mesh e2 edge;
  edge

let update_face mesh edge =
  let stop = prev mesh edge in
  print_int stop;flush stdout;
  let rec loop current_edge =
    set_face mesh current_edge edge;
    if current_edge <> stop then loop (next mesh current_edge)
  in
  loop edge

let connect mesh e1 e2 =
  if face mesh e1 <> face mesh e2 then
    invalid_arg "connect: try to connect 2 edges of to different faces.";
  let opposit_edge = connect_half mesh (prev mesh e2) (next mesh e1) in
  let edge = connect_half mesh e1 e2 in
  set_opposit mesh edge opposit_edge;
  set_opposit mesh opposit_edge edge;
  update_face mesh edge;
  update_face mesh opposit_edge;
  edge
  
