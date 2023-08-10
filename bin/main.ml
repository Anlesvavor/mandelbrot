let x1 = -2.0
;;
let x2 = 1.0
;;
let y1 = -1.5
;;
let y2 = 1.0
;;
let step = 0.0025

let mandelbrot_canvas =
  List.map (fun x ->
      List.map (fun y -> x, y) (Mandelbrot.range y1 y2 step)
    ) (Mandelbrot.range x1 x2 step)
  |> List.flatten
  |> List.map (fun (re, im) -> ((re, im), Mandelbrot.is_in_mandelbrot {re; im}))
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

  let print (t : t) =
    print_string @@ Printf.sprintf "{min_x = %f; max_x = %f; min_y = %f; max_y: %f}"
      t.min_x t.max_x t.min_y t.max_y
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

mandelbrot_canvas
|> List.filter (fun (_, b) -> b)
|> List.map (fun ((re, im), _) -> (re, im))
|> bounding_rect
|> BoundingRect.print
;;

let main () =
  let b_rect =
    mandelbrot_canvas
    |> List.filter (fun (_, b) -> b)
    |> List.map (fun ((re, im), _) -> (re, im))
    |> bounding_rect
  in
  let width = int_of_float @@ (b_rect.max_x -. b_rect.min_x) /. step in
  let height = int_of_float @@ (b_rect.max_y -. b_rect.min_y) /. step in
  let open Graphics in
  let resolution = Printf.sprintf " %dx%d" width height in
  open_graph resolution;
  set_window_title @@ Printf.sprintf "Mandelbrot set: %s" resolution;
  mandelbrot_canvas
  |> List.map (fun ((x, y), b) ->
      let offset_x = (-.b_rect.min_x) /. step in
      let offset_y = (-.b_rect.min_y) /. step in
      let x' = int_of_float @@ (x /. step +. offset_x) in
      let y' = int_of_float @@ (y /. step +. offset_y) in
      ((x', y'), b))
  |> List.iter (fun ((x, y), b) ->
      if b
      then set_color @@ rgb 0 0 0
      else set_color @@ rgb 255 255 255
      ;
      plot x y
    )
  ;
  ignore (read_key () : char)
;;

let _ = main ()
;;
