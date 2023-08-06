type canvasparam =
  { width : float
  ; height : float
  ; step_h : float
  ; step_w : float
  ; offset_x : float
  ; offset_y : float
  }
;;

let canvas (params : canvasparam) : (float * float) list =
  let ws = List.init
      (int_of_float (params.width /. params.step_w))
      (fun i -> ((float_of_int i) *. params.step_w) +. params.offset_x)
  in
  let hs = List.init
      (int_of_float (params.height /. params.step_h))
      (fun i -> ((float_of_int i) *. params.step_h) +. params.offset_y)
  in
  List.map (fun a -> List.map (fun b -> (a, b)) ws) hs
  |> List.flatten
;;
