open Graphics
open Gg
open Mesh

let perspective_matrix fov aspect near far =
  let h = 2. *. near *. tan (fov /. 2.) in
  let w = aspect *. h in
  M4.persp
    ~left: (-.w /. 2.)
    ~right: (w /. 2.)
    ~bot: (-.h /. 2.)
    ~top: (h /. 2.)
    ~near
    ~far

let pi = atan 1. *. 4.

let look_at ~position ~target ~up =
  let z_axis = V3.unit (V3.sub target position) in
  let x_axis = V3.unit (V3.cross up z_axis) in
  let y_axis = V3.cross z_axis x_axis in
  M4.v
    (V3.x x_axis) (V3.y x_axis) (V3.z x_axis) (-. (V3.dot x_axis position))
    (V3.x y_axis) (V3.y y_axis) (V3.z y_axis) (-. (V3.dot y_axis position))
    (V3.x z_axis) (V3.y z_axis) (V3.z z_axis) (-. (V3.dot z_axis position))
    0.            0.            0.            1.

    
let v x y z = V4.v x y z 1.

let vertices =
  [|
    v (-1.) (-1.) (-1.);
    v 1. (-1.) (-1.);
    v (-1.) (1.) (-1.);
    v 1. 1. (-1.);
    v (-1.) (-1.) 1.;
    v 1. (-1.) 1.;
    v (-1.) (1.) 1.;
    v 1. 1. 1.;
    v 0. 0. 0.
  |]

let faces =
  [
    [|0; 1; 3; 2|];
    [|0; 2; 6; 4|];
    [|0; 4; 5; 1|];
    [|7; 6; 2; 3|];
    [|7; 3; 1; 5|];
    [|6; 4; 5; 7|];
  ]

let mesh =
  new_mesh vertices faces
    

let () =
  open_graph "";
  let x = ref 0. in
  let y = ref 0. in
  let z = ref (-15.) in
  let theta = ref 0. in
  let t = ref (Unix.gettimeofday ()) in
  let frame_cpt = ref 0 in
  let fps = ref 0. in
  
  let width = float (size_x ()) /. 2. in

  let height = float (size_y ()) /. 2. in

  let perspective_matrix =
    perspective_matrix ( pi /. 4.) (width /. height) 0.01 1.
  in

  display_mode false;
  set_line_width 3;
  let rec loop () =
    clear_graph ();
    theta := !theta +. 0.001;
    x := 4. *. sin !theta;
    y := 1. +. 4. *. sin !theta;
    z := 4. *. cos !theta;
    let camera_matrix =
      M4.mul
        perspective_matrix
        (look_at ~position: (V3.v !x !y !z) ~target: V3.zero ~up: V3.oy)
    in
    let proj vertex =
      let v = V4.ltr camera_matrix vertex in
      let w = V4.w v in
      let x = int_of_float ((V4.x v) /. w *. width +.width) in
      let y = int_of_float ((V4.y v) /. w *. height +. height) in
      x, y
    in
    iter_edges
      (fun v1 v2 ->
         let x1, y1 = proj v1 in
         let x2, y2 = proj v2 in
         moveto x1 y1;
         lineto x2 y2
      )
      mesh;
    if !frame_cpt = 1000 then begin
      frame_cpt := 0;
      let new_t = Unix.gettimeofday () in
      let diff = new_t -. !t in
      t := new_t;
      fps := 1000. /. diff
    end else begin
      incr frame_cpt
    end;
    moveto 10 500;
    draw_string (Printf.sprintf "FPS = %.2f" !fps);
    synchronize ();
    if not (key_pressed ()) then loop ()
  in
  loop ()
