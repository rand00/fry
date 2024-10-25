open Lwt_react
open Fry_core

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
              acc +. f ~i:env_i |> Wavelet.clamp_float 0. 1.
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


