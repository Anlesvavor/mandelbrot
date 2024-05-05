module Params = struct
  type t =
    { x_domain: (float * float)
    ; y_domain: (float * float)
    ; resolution: (int * int)
    ; limit: int
    }

  let init ~x_domain ~y_domain ~resolution ~limit : t =
    { x_domain; y_domain; resolution; limit }

  let print (t : t) : string =
    let x1, x2 = t.x_domain in
    let y1, y2 = t.y_domain in
    (* let w, h = t.resolution in *)
    (* Printf.sprintf "{x_d:%f,%f;y_d:%f,%f;res:(%d, %d);l:%d} " x1 x2 y1 y2 w h t.limit *)
    Printf.sprintf "-xa %.20f -xb %.20f -ya %.20f -yb %.20f -l %d" x1 x2 y1 y2 t.limit
end
;;

let mandelbrot_canvas (params : Params.t) : ((float * float) * bool) list =
  let x1, x2 = params.x_domain in
  let y1, y2 = params.y_domain in
  let w, h = params.resolution in
  let y_step = (y2 -. y1) /. (float_of_int h) in
  let x_step = (x2 -. x1) /. (float_of_int w) in
  List.map (fun x ->
      List.map (fun y -> x, y) (Mandelbrot.range y1 y2 y_step)
    ) (Mandelbrot.range x1 x2 x_step)
  |> List.flatten
  |> List.map (fun (re, im) -> ((re, im), Mandelbrot.is_in_mandelbrot params.limit {re; im}))
;;

let draw
    (params : Params.t)
    (mouse_pos : (int * int))
  : unit =
  let x1, x2 = params.x_domain in
  let y1, y2 = params.y_domain in
  let width, height = params.resolution in
  let canvas = mandelbrot_canvas params in
  let points =
    canvas
    |> List.filter_map (fun ((x, y), is_in_set) ->
        let ratio v min_v max_v = (v -. min_v) /. (max_v -. min_v) in
        let offset_x = ratio x x1 x2  in
        let offset_y = ratio y y1 y2  in
        let x' = int_of_float @@ (offset_x *. float_of_int width) in
        let y' = int_of_float @@ (offset_y *. float_of_int height) in
        if is_in_set
        then Some (x', y')
        else None
      )
    |> Array.of_list
  in
  let info_bar = Printf.sprintf "Re: [%.20f, %.20f]; Im: [%.20fi, %.20fi]"
      x1 x2 y1 y2
  in
  let resolution = Printf.sprintf " %dx%d" width height in
  Graphics.open_graph resolution;
  Graphics.set_window_title @@ Printf.sprintf "Mandelbrot set: %s" resolution;
  Graphics.set_color @@ Graphics.rgb 0 0 0;
  Graphics.plots points;
  Graphics.set_color @@ Graphics.rgb 0 0 255;
  Graphics.fill_rect 0 0 width 15;
  Graphics.set_color @@ Graphics.rgb 255 128 128;
  Graphics.draw_string @@ info_bar;
  let mx, my = mouse_pos in
  Graphics.set_color @@ Graphics.rgb 0 255 0;
  Graphics.fill_circle mx my 2;
  Graphics.draw_circle (width/2) (height/2) 5;
  Graphics.moveto (width/2) height;
  Graphics.lineto (width/2) 0;
  Graphics.moveto 0 (height/2);
  Graphics.lineto width (height/2);
;;

(* let mouse_click () : (int * int) option = *)
(*   let e = Graphics.wait_next_event [Button_down] in *)
(*   if e.button *)
(*   then Some (e.mouse_x, e.mouse_y) *)
(*   else None *)
(* ;; *)

(* let key_press () : char option = *)
(*   let e = Graphics.wait_next_event [Key_pressed] in *)
(*   if e.keypressed *)
(*   then Some (e.key) *)
(*   else None *)
(* ;; *)

module Event = struct
  type t = Click of (int * int) | Key of char | None

  let read_input () : t =
    let e = Graphics.wait_next_event [Button_down; Key_pressed] in
    if e.button
    then Click (e.mouse_x, e.mouse_y)
    else if e.keypressed
    then Key e.key
    else None

end

let update_params
    (mouse_coord : int * int)
    (limit_increase : int)
    (zoom_factor: float)
    (params : Params.t)
  : Params.t =
  let x1, x2 = params.x_domain in
  let y1, y2 = params.y_domain in
  let width, height = params.resolution in
  let m_x, m_y = mouse_coord in
  let p_x = float_of_int m_x /. float_of_int width in
  let p_y = float_of_int m_y /. float_of_int height in
  let _ = print_string @@ Printf.sprintf "%f,%f " p_x p_y in
  let x_domain =
    let canvas_width = x2 -. x1 in
    let offset_x = x1 +. (canvas_width *. p_x) in
    Mandelbrot.zoom_tuple zoom_factor params.x_domain offset_x
  in
  let y_domain =
    let canvas_height = y2 -. y1 in
    let offset_y = y1 +. (canvas_height *. p_y) in
    Mandelbrot.zoom_tuple zoom_factor params.y_domain offset_y
  in
  Params.init
    ~x_domain
    ~y_domain
    ~resolution: params.resolution
    ~limit: (params.limit + limit_increase)
;;

let initial_params =
  Params.init
    ~x_domain: (-2.0, 2.0)
    ~y_domain: (-2.0, 2.0)
    ~resolution: (500, 500)
    ~limit: 100
;;

(* let string_of_pair (pair : (int * int)) : string = *)
(*   let x, y = pair in *)
(*   Printf.sprintf "(%d,%d)" x y *)
(* ;; *)

let main (initial_params : Params.t) =
  let params = ref initial_params in
  let w, h = !params.resolution in
  let mouse_pos = ref (w/2, h/2) in
  let default_zoom_factor = 0.5 in
  let zoom_factor = ref default_zoom_factor in
  while true do
    draw !params !mouse_pos;
    print_string @@ Params.print !params;
    print_newline ();
    let () = params := match (Event.read_input ()) with
        | Click mp ->
          Graphics.moveto 10 h;
          Graphics.set_color @@ Graphics.rgb 0 0 0;
          Graphics.draw_string "Processing...";
          mouse_pos := mp;
          (* print_string @@ string_of_pair mp; *)
          update_params !mouse_pos 0 !zoom_factor !params;
        | Key '+' -> update_params !mouse_pos 10 1.0 !params;
        | Key '-' -> update_params !mouse_pos (-10) 1.0 !params;
        | _ -> !params;
    in
    ()
  done
;;

let () =
  let usage_msg = "
    mandelbrotset
    -w int
    -h int
    -xa float
    -xb float
    -ya float
    -yb float
    -l int
   "
  in
  let w, h = initial_params.resolution in
  let w = ref w in
  let h = ref h in
  let x1, x2 = initial_params.x_domain in
  let y1, y2 = initial_params.y_domain in
  let x1 = ref x1 in
  let x2 = ref x2 in
  let y1 = ref y1 in
  let y2 = ref y2 in
  let l = initial_params.limit in
  let l = ref l in
  let speclist =
    [("-w", Arg.Set_int w, "Window width")
    ;("-h", Arg.Set_int h, "Window height")
    ;("-xa", Arg.Set_float x1, "Lower bound of the Real domain")
    ;("-xb", Arg.Set_float x2, "Upper bound of the Real domain")
    ;("-ya", Arg.Set_float y1, "Lower bound of the Imaginary domain")
    ;("-yb", Arg.Set_float y2, "Upper bound of the Imaginary domain")
    ;("-l", Arg.Set_int l, {|Iterations before the computed function gets "tired"|})
    ]
  in
  Arg.parse speclist (fun _ -> ()) usage_msg;
  Params.init
    ~x_domain: (!x1, !x2)
    ~y_domain: (!y1, !y2)
    ~resolution: (!w, !h)
    ~limit: !l
  |> main
;;
