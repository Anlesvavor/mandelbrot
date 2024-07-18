module Params = struct
  type mode = Bw | Color

  type t =
    { x_domain: (float * float)
    ; y_domain: (float * float)
    ; resolution: (int * int)
    ; limit: int
    ; mode: mode
    }

  let init ~x_domain ~y_domain ~resolution ~limit ~mode : t =
    { x_domain; y_domain; resolution; limit; mode }

  let print (t : t) : string =
    let x1, x2 = t.x_domain in
    let y1, y2 = t.y_domain in
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

let matrix_map (f : ('a -> 'b)) (matrix : 'a array array) : 'b array array =
  Array.map (fun row -> Array.map (fun i -> f i) row) matrix
;;

let mandelbrot_iterations_canvas (params : Params.t) : int array array =
  let x1, x2 = params.x_domain in
  let y1, y2 = params.y_domain in
  let w, h = params.resolution in
  let y_step = (y2 -. y1) /. (float_of_int h) in
  let x_step = (x2 -. x1) /. (float_of_int w) in
  Array.map (fun im ->
      Array.map (fun re ->
          Mandelbrot.iterations_before_scaping params.limit {re; im}
        ) (Array.of_list @@ Mandelbrot.range y1 y2 y_step)
    ) (Array.of_list @@ Mandelbrot.range x1 x2 x_step)
;;

let hsl_to_rgb h s l =
  let chroma = (1.0 -. (Float.abs ((2.0 *. l) -. 1.0)) ) *. s in
  let h' = (float_of_int h) /. 60.0 in
  let x = chroma *. (1.0 -. (Float.abs ((Float.rem h' 2.0) -. 1.0))) in
  let (r, g, b) = match h' with
    | n when 0.0 <= n && n <= 1.0 -> (chroma, x, 0.0)
    | n when 1.0 <= n && n <= 2.0 -> (x, chroma, 0.0)
    | n when 2.0 <= n && n <= 3.0 -> (0.0, chroma, x)
    | n when 3.0 <= n && n <= 4.0 -> (0.0, x, chroma)
    | n when 4.0 <= n && n <= 5.0 -> (x, 0.0, chroma)
    | n when 5.0 <= n && n <= 6.0 -> (chroma, 0.0, x)
    | _ -> failwith "not valid h"
  in
  let correction_m c =
    ((c +. (l -. (chroma /. 2.0))) *. 255.0)
    |> Float.round
    |> int_of_float
  in
  (correction_m r, correction_m g, correction_m b)
;;

let rgb_to_int r g b = (r * (256 * 256)) + (g * (256)) + b

let factal_pallete =
  let num_shades = 360 in
  List.(
    init num_shades (fun x ->
        let ratio = (float_of_int x) /. (float_of_int num_shades) in
        let hue = (((float_of_int (120 - 60)) *. ratio) +. 60.0) |> Float.round |> int_of_float in
        (* let luminisity = (float_of_int (num_shades * x)) /. (float_of_int num_shades) in *)
        (* let r, g, b = hsl_to_rgb hue 0.90 luminisity in *)
        let r, g, b = hsl_to_rgb hue 1.0 0.55 in
        (* let r, g, b = hsl_to_rgb hue (1.0/.ratio) ratio in *)
        (* (r * (256 * 256)) + (g * (256)) + b *)
        rgb_to_int r g b
      )
    |> rev
  ) |> Array.of_list
;;

(* let color_up = *)

let blue_gradient =
  let num_shades = 4098 in
  List.(
    init num_shades (fun x -> (x * 255) / num_shades )
    |> rev
  ) |> Array.of_list
;;

let x_gradient =
  let num_shades = 256 * 256 in
  List.(
    init num_shades (fun x ->
        if x <= 1
        then rgb_to_int 0 0 0
        else
          Float.(
            let t = pow ((of_int x) /. (of_int num_shades)) 3.5 in
            (* let x' = x |>  of_int |> (fun x -> mod_float (1.5 *. x) (of_int num_shades) ) in *)
            (* let t = pow ((x') /. (of_int num_shades)) 2.0 in *)
            let hue = to_int (120.0 +. ((120.0) *. (1.0 -. t))) in
            (* let r, g, b = hsl_to_rgb hue 0.5 0.5 in *)
            let r, g, b = hsl_to_rgb hue 0.5 (t) in
            rgb_to_int r g b
          )
      )
    |> rev
  ) |> Array.of_list
;;

let color_of_iteration (limit : int) (done_iterations : int) : int =
  let x = 20 in
  let gradient = match x with
    | 0 -> blue_gradient
    | 1 -> factal_pallete
    | _ -> x_gradient
  in
  let gradient_size = Array.length gradient in
  let color_index = (done_iterations * (gradient_size -1)) / limit in
  let assigned_color = Array.get gradient color_index in
  assigned_color
;;

let draw
    (params : Params.t)
    (mouse_pos : (int * int))
  : unit =
  let x1, x2 = params.x_domain in
  let y1, y2 = params.y_domain in
  let width, height = params.resolution in
  let limit = params.limit in
  let info_bar = Printf.sprintf "Re: [%.20f, %.20f]; Im: [%.20fi, %.20fi]"
      x1 x2 y1 y2
  in
  let resolution = Printf.sprintf " %dx%d" width height in
  Graphics.open_graph resolution;
  let () = match params.mode with
    | Bw ->
      Graphics.set_color @@ Graphics.rgb 0 0 0;
      let canvas = mandelbrot_canvas params in
      let points =
        canvas
        |> List.filter_map (fun ((x, y), b) ->
            if b
            then
              let ratio v min_v max_v = (v -. min_v) /. (max_v -. min_v) in
              let x' =
                let offset_x = ratio x x1 x2 in
                int_of_float @@ (offset_x *. float_of_int width)
              in
              let y' =
                let offset_y = ratio y y1 y2 in
                int_of_float @@ (offset_y *. float_of_int height) in
              Some (x', y')
            else None
          )
        |> Array.of_list
      in
      Graphics.plots points
    | Color ->
      let colors = (mandelbrot_iterations_canvas params) |> matrix_map (color_of_iteration limit) in
      Graphics.draw_image (Graphics.make_image colors) 0 0
  in
  Graphics.set_window_title @@ Printf.sprintf "Mandelbrot set: %s" resolution;
  Graphics.set_color @@ Graphics.rgb 0 0 255;
  Graphics.fill_rect 0 0 width 15;
  Graphics.set_color @@ Graphics.rgb 255 128 128;
  Graphics.draw_string @@ info_bar;
  let _mx, _my = mouse_pos in ()
;;

module Event = struct
  type t = Click of (int * int)
         | Key of char
         | None

  let read_input () : t =
    let e = Graphics.wait_next_event [Button_down; Key_pressed] in
    if e.button
    then Click (e.mouse_x, e.mouse_y)
    else if e.keypressed
    then Key e.key
    else None
  ;;

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
  let m_x, m_y =
    let x, y = mouse_coord in
    (height - y, x)
  in
  let p_x = float_of_int m_x /. float_of_int width in
  let p_y = float_of_int m_y /. float_of_int height in
  let _ = print_string @@ Printf.sprintf "%f,%f " p_x p_y in
  let x_domain =
    let canvas_width = x2 -. x1 in
    let () = Printf.printf "canvas_width: %f" canvas_width in
    let () = print_newline () in
    let offset_x = x1 +. (canvas_width *. p_x) in
    let () = Printf.printf "offset_x: %f" offset_x in
    let () = print_newline () in
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
    ~mode: params.mode
;;

let initial_params =
  Params.init
    ~x_domain: (-2.0, 2.0)
    ~y_domain: (-2.0, 2.0)
    ~resolution: (500, 500)
    ~limit: 100
    ~mode: Bw
;;

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
          update_params !mouse_pos 0 !zoom_factor !params;
        | Key '+' -> update_params !mouse_pos 50 1.0 !params;
        | Key '-' -> update_params !mouse_pos (-50) 1.0 !params;
        | Key '=' -> update_params !mouse_pos 50 (1.0 /. !zoom_factor) !params;
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
    -c flag color
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
  let mode = ref Params.Color in
  let speclist =
    [("-w", Arg.Set_int w, "Window width")
    ;("-h", Arg.Set_int h, "Window height")
    ;("-xa", Arg.Set_float x1, "Lower bound of the Real domain")
    ;("-xb", Arg.Set_float x2, "Upper bound of the Real domain")
    ;("-ya", Arg.Set_float y1, "Lower bound of the Imaginary domain")
    ;("-yb", Arg.Set_float y2, "Upper bound of the Imaginary domain")
    ;("-l", Arg.Set_int l, {|Iterations before the computed function gets "tired"|})
    ;("-c", Arg.Unit (fun () -> mode := Params.Bw), {|Uses Color|})
    ]
  in
  Arg.parse speclist (fun _ -> ()) usage_msg;
  Params.init
    ~x_domain: (!x1, !x2)
    ~y_domain: (!y1, !y2)
    ~resolution: (!w, !h)
    ~limit: !l
    ~mode: !mode
  |> main
;;
