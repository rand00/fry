open Lwt_react

(** Example use*)
(* let _ = *)
(*   let fps = 30. in *)
(*   let envelope ~i ~v =  *)
(*     (\*> Note: this should go from 0. to 1. to 0., in 1 sec *)
(*       where i starts at 0 on new input event *)
(*     *\) *)
(*     let i = Float.pi *. 2. *. float i /. fps in *)
(*     (1. +. sin (i -. Float.pi /. 2.)) /. 2. *)
(*   in *)
(*   rhythm_e *)
(*   |> Fry.Envelope.create ~tick_e ~f:envelope *)

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

let sine ~length ~i ~v =
  let i_f = float i in
  let i_pct = i_f /. length in
  if i_pct >= 1. then 0. else 
    let angle = i_pct *. Float.pi *. 2. in
    (1. +. sin (angle -. Float.pi /. 2.)) /. 2.

let cut_attack length f ~i ~v =
  f ~i:(truncate length + i) ~v

let cut_decay length f ~i ~v =
  let i = if i >= truncate length then Int.max_int else i in
  f ~i ~v

let apply op f g ~i ~v = op (f ~i ~v) (g ~i ~v)

let add f g = apply (+.) f g
let mul f g = apply ( *. ) f g
let max f g = apply Float.max f g
let min f g = apply Float.min f g

let sum fs ~i ~v =
  List.fold_left (fun acc f -> acc +. f ~i ~v) 0. fs

let pure x ~i ~v = x

let cmul c f = mul f @@ pure c
let cmin c f = min f @@ pure c

(** Note that this depends on 'f' being pure*)
let normalize_on_i ~length ~v_static f =
  let vs = List.init (truncate length) (fun i -> f ~i ~v:v_static) in
  let max_v = List.fold_left Float.max Float.min_float vs in
  fun ~i ~v -> 
    f ~i ~v /. max_v

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

let adsr ~a ~d ~s ~r = [ a; d; s; r ] |> point_list

let trace tag f ~i ~v =
  let r = f ~i ~v in
  Printf.eprintf "envelope: i = %d, %s = %.2f%!\n" i tag r;
  r



