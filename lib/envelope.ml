open Lwt_react

let create ~tick_e ~f e =
  let s = 
    e
    |> Event.add_index
    |> E.map Option.some
    |> S.hold ~eq:Eq.never None
  in
  S.sample (fun _ v -> v) tick_e s
  |> E.fold (fun (last_i, _env, env_i as acc) -> function
    | None -> acc
    | Some (i, v) ->
      let env_i = if Int.equal last_i i then succ env_i else 0 in
      let env = f ~i:env_i ~v in
      i, env, env_i
  ) (-1, 0., -1)
  |> E.map (fun (_, env, _) -> env)
  |> S.hold ~eq:CCFloat.equal 0.

let to_int f = f |> Float.round |> Float.to_int

module Inf = struct

  let of_finite ~length f ~i ~v =
    let i = i mod to_int length in
    f ~i ~v

end

let sine ~length ~i ~v =
  let i_f = float i in
  let i_pct = i_f /. length in
  if i_pct >= 1. then 0. else 
    let angle = i_pct *. Float.pi *. 2. in
    (1. +. sin (angle -. Float.pi /. 2.)) /. 2.

let cut_start length f ~i ~v =
  f ~i:(to_int length + i) ~v

let cut_end length f ~i ~v =
  let i = if i >= to_int length then Int.max_int else i in
  f ~i ~v

let apply op f g ~i ~v = op (f ~i ~v) (g ~i ~v)

let add f g = apply (+.) f g
let sub f g = apply (-.) f g
let mul f g = apply ( *. ) f g
let max f g = apply Float.max f g
let min f g = apply Float.min f g

let phase ~length ~shift f ~i ~v =
  let i = (i + to_int shift) mod to_int length in
  f ~i ~v

let sum fs ~i ~v =
  List.fold_left (fun acc f -> acc +. f ~i ~v) 0. fs

let pure x ~i ~v = x

let cmul c f = mul f @@ pure c
let cmin c f = min f @@ pure c

(** Note that this depends on 'f' being pure*)
let normalize_on_i ?(cut_negative=true) ~length ~v_static f =
  let vs = List.init (to_int length) (fun i -> f ~i ~v:v_static) in
  let min_v, max_v = List.fold_left (fun (min_v, max_v) v ->
    Float.min min_v v, Float.max max_v v
  ) (Float.max_float, Float.min_float) vs
  in
  let range_v = max_v -. min_v in
  if min_v >= 0. then
    (*We don't move envelopes that don't hit 0. down*)
    fun ~i ~v -> f ~i ~v /. max_v
  else (
    if cut_negative then
      (*For understandable handling of messy envelopes*)
      fun ~i ~v -> Float.max 0. (f ~i ~v) /. max_v
    else 
      (*If envelope is negative, we move envelope up*)
      fun ~i ~v -> (f ~i ~v -. min_v) /. range_v
  )

let point_list l ~i ~v =
  (*> Note: depends on i being in between v2.x and v2'.x*)
  let interp_aux i v2 v2' =
    let diff_x = fst v2' -. fst v2 in
    let diff_y = snd v2' -. snd v2 in
    let i_pct = (i -. fst v2) /. diff_x in
    let y_rel = i_pct *. diff_y in
    snd v2 +. y_rel
  in
  let i = float i in
  let rec aux ~vec_prev = function
    | [] -> 0.
    | vec_rel :: rest -> 
      let x = fst vec_prev +. fst vec_rel in
      let vec_abs = x, snd vec_rel in
      if i <= x then
        interp_aux i vec_prev vec_abs
      else
        aux ~vec_prev:vec_abs rest
  in
  aux ~vec_prev:(0., 0.) l

let adsr a d s r = [ a; d; s; r ] |> point_list

let trace tag f ~i ~v =
  let r = f ~i ~v in
  Printf.eprintf "envelope: i = %d, %s = %.2f%!\n" i tag r;
  r



