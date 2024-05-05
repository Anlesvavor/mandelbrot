
module ComplexDecimal = struct
  type t = { re: Decimal.t; im: Decimal.t }

  let (+) = Decimal.(+);;
  let (-) = Decimal.(-);;
  let (~-) = Decimal.(~-);;
  let ( * ) = Decimal.( * );;

  let zero = { re = 0.0m; im = 0.0m }
  let one = { re = 1.0m; im = 0.0m }
  let i = { re = 0.0m; im = 1.0m }

  let add x y = { re = x.re + y.re; im = x.im + y.im }

  let sub x y = { re = x.re - y.re; im = x.im - y.im }

  let neg x = { re = ((~-) x.re); im = ((~-) x.im) }

  let conj x = { re = x.re; im = ((~-) x.im) }

  let mul x y = { re = x.re * y.re - x.im * y.im;
                  im = x.re * y.im + x.im * y.re }

  let norm2 x = x.re * x.re + x.im * x.im

  let hypot (a : Decimal.t) (b : Decimal.t) : Decimal.t =
    let open Decimal in sqrt((a * a) + (b * b))

  let norm x = hypot x.re x.im

  (* let div x y = *)
  (*   if abs_float y.re >= abs_float y.im then *)
  (*     let r = y.im / y.re in *)
  (*     let d = y.re + r * y.im in *)
  (*     { re = (x.re + r * x.im) / d; *)
  (*       im = (x.im - r * x.re) / d } *)
  (*   else *)
  (*     let r = y.re / y.im in *)
  (*     let d = y.im + r * y.re in *)
  (*     { re = (r * x.re + x.im) /d; *)
  (*       im = (r * x.im - x.re) /d } *)

  (* let inv x = div one x *)
end


let mandelbrot (c : ComplexDecimal.t) (i : int) (z : ComplexDecimal.t) : ComplexDecimal.t =
  match i with
  | 0 -> ComplexDecimal.zero
  | _ -> ComplexDecimal.add (ComplexDecimal.mul z z) c
;;

let is_in_mandelbrot (limit : int) (c : ComplexDecimal.t) : bool =
  let mandelbrot' = mandelbrot c in
  let rec is_in_mandelbrot' fitness i c z =
    if fitness = 0
    then true (* function "tired" of searching, c must be part of the set *)
    else
    if (Decimal.(>) (ComplexDecimal.norm z) (Decimal.of_int 2))
    then false
    else
      let z' = mandelbrot' i z in
      is_in_mandelbrot' (pred fitness) (succ i) c z'
  in
  is_in_mandelbrot' limit 0 c ComplexDecimal.zero
;;

let range (lower_bound : Decimal.t) (upper_bound : Decimal.t) (step : Decimal.t) =
  let open Decimal in
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
      | Direction.Downwards -> -step
    in
    if current_position > upper_bound
    then acc
    else aux (current_position :: acc) (current_position + step')
  in
  aux [] lower_bound
  |> List.rev
;;

let zoom_tuple
    (z_factor : Decimal.t)
    (ab : Decimal.t * Decimal.t)
    (target : Decimal.t)
  : (Decimal.t * Decimal.t) =
  let open Decimal in
  let a, b = ab in
  let width = b - a in
  let middle = a + (width / 2.0m) in
  let delta = target - middle in
  let a', b' = a + delta, b + delta in
  let a'' = a' + ((middle - a') * (1.0m - z_factor)) in
  let b'' = middle + ((b' - middle) * z_factor) in
  (a'', b'')
;;

