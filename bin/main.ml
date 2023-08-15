module Params = struct
  type t =
    { x_domain: (float * float)
    ; y_domain: (float * float)
    ; step: float
    ; limit: int
    }

  let init ~x_domain ~y_domain ~step ~limit : t =
    { x_domain; y_domain; step; limit }

  let print (t : t) : string =
    let x1, x2 = t.x_domain in
    let y1, y2 = t.y_domain in
    Printf.sprintf "{x_d:%f,%f;y_d:%f,%f;s:%f;l:%d} " x1 x2 y1 y2 t.step t.limit
end
;;

let initial_params =
  Params.init ~x_domain: (-2.0, 1.0) ~y_domain: (-1.5, 1.0) ~step: 0.005 ~limit:25
;;

let mandelbrot_canvas (params : Params.t) : ((float * float) * bool) list =
  let x1, x2 = params.x_domain in
  let y1, y2 = params.y_domain in
  let step = params.step in
  List.map (fun x ->
      List.map (fun y -> x, y) (Mandelbrot.range y1 y2 step)
    ) (Mandelbrot.range x1 x2 step)
  |> List.flatten
  |> List.map (fun (re, im) -> ((re, im), Mandelbrot.is_in_mandelbrot params.limit {re; im}))
;;

module BoundingRect = struct
  type t =
    { min_x : float
    ; max_x : float
    ; min_y : float
    ; max_y : float
    }

  let init
      ?(min_x = Float.max_float)
      ?(max_x = Float.min_float)
      ?(min_y = Float.max_float)
      ?(max_y = Float.min_float)
      ()
    : t =
    { min_x; max_x; min_y; max_y }
  ;;

end

let bounding_rect (coords : (float * float) list) : BoundingRect.t =
  let rec aux (bounding_rect : BoundingRect.t) list = match list with
    | [] -> bounding_rect
    | (x, y) :: xs ->
      let min_x = if x < bounding_rect.min_x then x else bounding_rect.min_x in
      let min_y = if y < bounding_rect.min_y then y else bounding_rect.min_y in
      let max_x = if x > bounding_rect.max_x then x else bounding_rect.max_x in
      let max_y = if y > bounding_rect.max_y then y else bounding_rect.max_y in
      aux (BoundingRect.init ~min_x ~max_x ~min_y ~max_y ()) xs
  in
  aux (BoundingRect.init ()) coords
;;

let loop (params : Params.t) : Params.t =
  let step = params.step in
  let canvas = mandelbrot_canvas params in
  let b_rect =
    canvas
    |> List.filter (fun (_, b) -> b)
    |> List.map (fun ((re, im), _) -> (re, im))
    |> bounding_rect
  in
  let x1, x2 = params.x_domain in
  let y1, y2 = params.y_domain in
  let width = int_of_float @@ (x2 -. x1) /. params.step in
  let height = int_of_float @@ (y2 -. y1) /. params.step in
  let resolution = Printf.sprintf " %dx%d" width height in
  Graphics.open_graph resolution;
  Graphics.set_window_title @@ Printf.sprintf "Mandelbrot set: %s" resolution;
  Graphics.set_color @@ Graphics.rgb 0 0 0;
  let canvas = canvas
               |> List.filter_map (fun ((x, y), is_in_set) ->
                   let offset_x = -.b_rect.min_x /. step in
                   let offset_y = -.b_rect.min_y /. step in
                   let x' = int_of_float @@ (x /. step +. offset_x) in
                   let y' = int_of_float @@ (y /. step +. offset_y) in
                   if is_in_set
                   then Some (x', y')
                   else None
                 )
               |> Array.of_list
  in Graphics.plots canvas;
  let e = Graphics.wait_next_event [Button_down] in
  let mouse_coord = if e.button
    then Some (e.mouse_x, e.mouse_y)
    else None
  in
  let params = match mouse_coord with
    | Some (m_x, m_y) ->
      let p_x = float_of_int m_x /. float_of_int width in
      let p_y = float_of_int m_y /. float_of_int height in
      let _ = print_string @@ Printf.sprintf "%f,%f " p_x p_y in
      let x_domain =
        let x1, x2 = params.x_domain in
        let canvas_width = x2 -. x1 in
        let offset_x = x1 +. (canvas_width *. p_x) in
        Mandelbrot.zoom_tuple 0.5 params.x_domain offset_x
      in
      let y_domain =
        let y1, y2 = params.y_domain in
        let canvas_height = y2 -. y1 in
        let offset_y = y1 +. (canvas_height *. p_y) in
        Mandelbrot.zoom_tuple 0.5 params.y_domain offset_y
      in
      let step = step /. 2.0 in
      Params.init ~x_domain ~y_domain ~step ~limit: (int_of_float @@ float_of_int params.limit *. 1.2)
    | None ->
      let _ = print_string "? " in
      params
  in
  params
;;

let main () =
  let params = ref initial_params in
  while true do
    print_string @@ Params.print !params;
    print_newline ();
    params := loop !params
  done
;;

main ()
