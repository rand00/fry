open Lwt_react
open Fry_core

(*> Note that clamping on [0;1] happens when acc_envs = Some _*)
let create' ?(acc_envs=None) ~tick_e ~wavelet_s e =
  let s = 
    e
    |> Event.add_index
    |> E.map Option.some
    |> S.hold ~eq:Eq.never None
  in
  let sampled_s =
    S.l2 ~eq:Eq.never Tuple.mk2 wavelet_s s 
  in
  S.sample (fun _ v -> v) tick_e sampled_s
  |> E.fold (fun (last_evt_i, _env, env_i, envs as acc) -> function
    | _      , None -> acc (*< Note: before any event has happened*)
    | wavelet, Some (evt_i, v) ->
      let wavelet ~v ~i = wavelet ~i ~v in
      let wavelet_partial = wavelet ~v in
      (*< Note: as OCaml wants a specific param order*)
      let env_i = if Int.equal last_evt_i evt_i then succ env_i else 0 in
      let env_v, envs = match acc_envs with
        | None -> wavelet ~v ~i:env_i, []
        | Some length ->
          let envs =
            envs |> CCList.map (fun (f, prev_env_i) ->
              f, succ prev_env_i
            )
          in
          let envs = 
            if env_i = 0 then 
              (wavelet_partial, 0) :: envs
              |> CCList.take length
            else envs 
          in
          let env_acc =
            envs
            |> CCList.fold_left (fun acc (f, env_i) ->
              acc +. f ~i:env_i |> Wavelet.clamp_float 0. 1.
            ) 0.
          in
          env_acc, envs
      in
      evt_i, env_v, env_i, envs
  ) (-1, 0., -1, [])
  |> E.map (fun (_, env_v, _, _) -> env_v)
  |> S.hold ~eq:CCFloat.equal 0.

let create ?(acc_envs=None) ~tick_e ~wavelet e =
  let wavelet_s = S.const wavelet in
  create' ~acc_envs ~tick_e ~wavelet_s e

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


