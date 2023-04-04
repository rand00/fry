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

let sine ~fps ~secs ~i ~v =
  let i = Float.pi *. 2. *. secs *. float i /. fps in
  (1. +. sin (i -. Float.pi /. 2.)) /. 2.

