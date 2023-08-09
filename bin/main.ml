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
  |> List.map (fun (re, im) -> ((re, im), Mandelbrot.is_in_mandelbrot {re ; im}))

let main () =
  let width = int_of_float @@ (x2 -. x1) /. step in
  let height = int_of_float @@ (y2 -. y1) /. step in
  let open Graphics in
  let resolution = Printf.sprintf " %dx%d" width height in
  open_graph resolution;
  set_window_title @@ Printf.sprintf "Mandelbrot set: %s" resolution;
  mandelbrot_canvas
  |> List.map (fun ((x, y), v) ->
      let offset_x = 2.0 /. step in
      let offset_y = 1.0 /. step in
      let x' = int_of_float @@ (x /. step +. offset_x) in
      let y' = int_of_float @@ (y /. step +. offset_y) in
      ((x', y'), v))
  |> List.iter (fun ((x, y), v) ->
      if v
      then set_color (rgb 0 0 0)
      else set_color (rgb 255 255 255)
      ;
      plot x y
    )
  ;
  ignore (read_key () : char)
;;

let _ = main ()
