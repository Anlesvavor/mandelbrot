
let mandelbrot (i : int) (c : Complex.t) (z : Complex.t) : Complex.t =
  match i with
  | 0 -> Complex.zero
  | _ -> Complex.add (Complex.mul z z) c
;;

let is_in_mandelbrot (c : Complex.t) : bool =
  let rec is_in_mandelbrot' fitness i c z =
    if fitness = 0
    then (* function "tired" of searching, c must be part of the set *) true
    else
      let z' = (mandelbrot i c z) in
      if Complex.norm z > 2.0
      then false
      else is_in_mandelbrot' (pred fitness) (succ i) c z'
  in
  is_in_mandelbrot' 10 0 c Complex.zero
;;

let rec mandelbrot_seq ?(i : int = 0) (c : Complex.t) (z : Complex.t) : Complex.t Seq.node =
  let z = mandelbrot i c z in
  Seq.cons z (fun () -> mandelbrot_seq ~i:(succ i) c z) ()
;;

let head_or_fail = function
  | [] -> failwith "head_of_fail: fail!"
  | x :: _ -> x
;;

let iter_before_exceeding (value : Complex.t) (seq : unit -> Complex.t Seq.node) : int =
  seq
  |> Seq.take 10
  |> Seq.take_while (fun el -> el < value)
  |> Seq.length
;;

module Canvas = struct
  type params =
    { width : float
    ; height : float
    ; step_h : float
    ; step_w : float
    ; offset_x : float
    ; offset_y : float
    }
  ;;

  let canvas (params : params) : (float * float) list =
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

end
