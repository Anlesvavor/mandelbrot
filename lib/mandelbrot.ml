
let mandelbrot (c : Complex.t) (i : int) (z : Complex.t) : Complex.t =
  match i with
  | 0 -> Complex.zero
  | _ -> Complex.add (Complex.mul z z) c
;;

let is_in_mandelbrot (limit : int) (c : Complex.t) : bool =
  let mandelbrot' = mandelbrot c in
  let rec is_in_mandelbrot' fitness i c z =
    if fitness = 0
    then true (* function "tired" of searching, c must be part of the set *)
    else
    if Complex.norm z > 2.0
    then false
    else
      let z' = mandelbrot' i z in
      is_in_mandelbrot' (pred fitness) (succ i) c z'
  in
  is_in_mandelbrot' limit 0 c Complex.zero
;;

let iterations_before_scaping (limit : int) (c : Complex.t) : int =
  let mandelbrot' = mandelbrot c in
  let rec is_in_mandelbrot' fitness i c z =
    if fitness = 0
    then limit (* function "tired" of searching, c must be part of the set *)
    else
    if Complex.norm z > 2.0
    then limit - fitness
    else
      let z' = mandelbrot' i z in
      is_in_mandelbrot' (pred fitness) (succ i) c z'
  in
  is_in_mandelbrot' limit 0 c Complex.zero
;;

let range (lower_bound : float) (upper_bound : float) (step : float) =
  let module Direction = struct
    type t = Downwards | Upwards
  end in
  let direction = if upper_bound > lower_bound
    then Direction.Upwards
    else Direction.Downwards
  in
  let rec aux acc current_position =
    let step' = match direction with
      | Direction.Upwards -> step
      | Direction.Downwards -> -.step
    in
    if current_position > upper_bound
    then acc
    else aux (current_position :: acc) (current_position +. step')
  in
  aux [] lower_bound
  |> List.rev
;;

let zoom_tuple
    (z_factor : float)
    (ab : float * float)
    (target : float)
  : (float * float) =
  let a, b = ab in
  let width = b -. a in
  let middle = a +. (width /. 2.0) in
  let delta = target -. middle in
  let a', b' = a +. delta, b +. delta in
  let a'' = a' +. ((middle -. a') *. (1.0 -. z_factor)) in
  let b'' = middle +. ((b' -. middle) *. z_factor) in
  (a'', b'')
;;

