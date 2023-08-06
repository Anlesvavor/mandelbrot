let params : Mandelbrot.Canvas.params =
  { width = 4.0
  ; height = 4.0
  ; step_h = 0.005
  ; step_w = 0.005
  ; offset_x = -2.0
  ; offset_y = -2.0
  }
;;

(* let paint = function *)
(*   | 0 -> 255 *)
(*   | 1 -> 225 *)
(*   | 2 -> 210 *)
(*   | 3 -> 31 *)
(*   | 4 -> 15 *)
(*   | 5 -> 7 *)
(*   | 6 -> 3 *)
(*   | 7 -> 1 *)
(*   | _ -> 0 *)
(* ;; *)

let mandelbrot_canvas =
  Mandelbrot.Canvas.canvas params
  |> List.map (fun (re, im) -> ((re, im), Mandelbrot.is_in_mandelbrot {re ; im}))
;;

let main () =
  let width = (params.width /. params.step_w)
              |> int_of_float
              |> string_of_int
  in
  let height = (params.height /. params.step_h)
               |> int_of_float
               |> string_of_int
  in
  let open Graphics in
  Printf.sprintf " %sx%s" width height
  |> open_graph
  ;
  mandelbrot_canvas
  |> List.map (fun ((x, y), v) ->
      let x' = int_of_float ((x /. params.step_w) +. (-.params.offset_x /. params.step_w)) in
      let y' = int_of_float (y /. params.step_h +. (-.params.offset_y /. params.step_h)) in
      ((x', y'), v))
  |> List.iter (fun ((x, y), v) ->
      if v
      then set_color (rgb 255 255 255)
      else set_color (rgb 0 0 0)
      ;
      plot x y
    )
  ;
  ignore (read_key () : char)
;;

let _ = main ()
