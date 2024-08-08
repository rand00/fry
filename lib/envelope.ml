open Lwt_react

let clamp from to_ v =
  if v < from then from
  else if v > to_ then to_
  else v

(*> Note that clamping on [0;1] happens when acc_envs = Some _*)
let of_env_signal ?(acc_envs=None) ~tick_e ~f e =
  let s = 
    e
    |> Event.add_index
    |> E.map Option.some
    |> S.hold ~eq:Eq.never None
  in
  let sampled_s =
    S.l2 ~eq:Eq.never Tuple.mk2 f s 
  in
  S.sample (fun _ v -> v) tick_e sampled_s
  |> E.fold (fun (last_i, _env, env_i, envs as acc) -> function
    | _, None -> acc (*< Note: before any event has happened*)
    | f, Some (i, v) ->
      let f ~v ~i = f ~i ~v in
      let env_partial = f ~v in
      (*< Note: as OCaml wants a specific param order*)
      let env_i = if Int.equal last_i i then succ env_i else 0 in
      let env, envs = match acc_envs with
        | None -> f ~v ~i:env_i, []
        | Some length ->
          let envs =
            envs |> CCList.map (fun (f, prev_env_i) ->
              f, succ prev_env_i
            )
          in
          let envs = 
            if env_i = 0 then 
              (env_partial, 0) :: envs
              |> CCList.take length
            else envs 
          in
          let env =
            envs
            |> CCList.fold_left (fun acc (f, env_i) ->
              acc +. f ~i:env_i |> clamp 0. 1.
            ) 0.
          in
          env, envs
      in
      i, env, env_i, envs
  ) (-1, 0., -1, [])
  |> E.map (fun (_, env, _, _) -> env)
  |> S.hold ~eq:CCFloat.equal 0.

let create ?(acc_envs=None) ~tick_e ~f e =
  let f = S.const f in
  of_env_signal ~acc_envs ~tick_e ~f e

let to_int f = f |> CCFloat.round |> CCFloat.to_int

module Inf = struct

  let of_finite ?on_repeat ~length f ~i ~v =
    let length = to_int length in
    let i = i mod length in
    match on_repeat with
    | None -> f ~i ~v
    | Some g ->
      let i_repeat = i / length in
      g ~i_repeat ~i @@ f ~i ~v

end

(*spec for envelope functions:
  * they should all be finite by default
  * when an envelope ends, its value goes to 0.
*)

let sine ~length ~i ~v =
  let i_f = float i in
  let i_pct = i_f /. length in
  if i_pct >= 1. then 0. else 
    let angle = i_pct *. CCFloat.pi *. 2. in
    (1. +. sin (angle -. CCFloat.pi /. 2.)) /. 2.

let cut_start length f ~i ~v =
  f ~i:(to_int length + i) ~v

(*> goto warning: this relies on expectation that 'f Int.max' will be = 0.
    .. which is not the case for infinite waves; should just return 0. instead 
*)
let cut_end length f ~i ~v =
  let i = if i >= to_int length then Int.max_int else i in
  f ~i ~v

let zero ~i ~v = 0.
let one ~i ~v = 1.

let apply op f g ~i ~v = op (f ~i ~v) (g ~i ~v)

let add f g = apply (+.) f g
let sub f g = apply (-.) f g
let mul f g = apply ( *. ) f g
let max f g = apply CCFloat.max f g
let min f g = apply CCFloat.min f g
let gt  f g = apply (fun x y -> if x > y then 1. else 0.) f g 
let lt  f g = gt g f
let eq ~eps f g = apply (fun x y ->
  if x +. eps > y && x -. eps < y then 1. else 0.
) f g 

(*> Note: useful for composition over time*)
let delay ~n f ~i ~v =
  let n = to_int n in
  if i < n then
    0.
  else (*i >= n*)
    let i = i - n in
    f ~i ~v

(*> goto problem: this makes 'f' into an infinite envelope...
  * problem if casing on value of 'i':
    * either the function won't support finite envelopes,
    * or it wont support infinite ones
    * @idea; could check if 'f ~i ~v > 0.' and then expect it to be infinite if
      also 'i >= length'
*)
(*> goto support negative shifting*)
let phase ~length ~shift f ~i ~v =
  (*> Note: the reason for 'mod length' is that envelopes can be finite*)
  let i = (i + to_int shift) mod to_int length in
  f ~i ~v

(*> Note that modulating length will probably not accumulate to a precise
    envelope position relative to modulated bpm.
    Therefore you should probably keep the envelope length quite a lot shorter
    than the full length between beats, to avoid cutting off envelope midway
*)
let make_phase_correct () =
  let acc_diff_i = ref 0 in
  fun ~prev_length ~length f ~i ~v -> 
    let prev_length' = to_int prev_length in
    if prev_length' > 0 then (
      if i = 0 then
        acc_diff_i := 0
      else 
        let prev_env_i_pct = float i /. prev_length in
        let i' = prev_env_i_pct *. length |> to_int in
        let diff_i = i' - i in
        acc_diff_i := !acc_diff_i + diff_i;
        (* Printf.eprintf "phase-correct: i = %d, \ *)
        (*                 prev_length = %.2f, length = %.2f, \ *)
        (*                 prev_env_i_pct = %.2f, i' = %d, \ *)
        (*                 diff_i = %d, acc_diff_i = %d \n%!" *)
        (*   i prev_length length *)
        (*   prev_env_i_pct i' *)
        (*   diff_i !acc_diff_i *)
        (* ; *)
    );
    let i = i + !acc_diff_i in
    f ~i ~v

let sum fs ~i ~v = CCList.fold_left (fun acc f -> acc +. f ~i ~v) 0. fs

let pure x ~i ~v = x

let cmul c f = mul f @@ pure c
let cmin c f = min f @@ pure c

(** Note that this depends on 'f' being pure*)
let normalize_on_i ?(cut_negative=true) ~length ~v_static f =
  let vs = CCList.init (to_int length) (fun i -> f ~i ~v:v_static) in
  let min_v, max_v = vs |> CCList.fold_left (fun (min_v, max_v) v ->
    CCFloat.min min_v v, CCFloat.max max_v v
  ) (Float.max_float, CCFloat.min_value)
  in
  let range_v = max_v -. min_v in
  if min_v >= 0. then
    (*We don't move envelopes that don't hit 0. down*)
    fun ~i ~v -> f ~i ~v /. max_v
  else (
    if cut_negative then
      (*For understandable handling of messy envelopes*)
      fun ~i ~v -> CCFloat.max 0. (f ~i ~v) /. max_v
    else 
      (*If envelope is negative, we move envelope up*)
      fun ~i ~v -> (f ~i ~v -. min_v) /. range_v
  )

let points l ~i ~v =
  (*> Note: depends on i being in between v2.x and v2'.x*)
  let interp_aux i v2 v2' =
    let diff_x = fst v2' -. fst v2 in
    let diff_y = snd v2' -. snd v2 in
    if CCFloat.equal diff_x 0. then
      snd v2
    else 
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

let adsr a d s r = [ a; d; s; r ] |> points

let trace tag f ~i ~v =
  let r = f ~i ~v in
  Printf.eprintf "envelope: i = %d, %s = %.2f%!\n" i tag r;
  r

module Trigger = struct

  (*Usage example:*)
  (* let _ = *)
  (*   env_s |> Fry.Envelope.Trigger.on 0.8 *)
  (*   |> E.map (function *)
  (*     | `Up -> foo *)
  (*     | `Down -> bar *)
  (*   ) *)

  let on (trigger_v:float) env_s =
    let trigger_aux acc v =
      match acc with
      | None -> Some (None, v)
      | Some (_out, last_v) ->
        if last_v < trigger_v && trigger_v < v then
          Some (Some `Up, v)
        else if v < trigger_v && trigger_v < last_v then
          Some (Some `Down, v)
        else
          Some (None, v)
    in
    env_s
    |> S.changes
    |> E.fold trigger_aux None
    |> E.fmap (function
      | None -> None
      | Some (out, _) -> out
    )
  
end 


