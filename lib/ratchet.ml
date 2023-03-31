open Lwt_react

(*goto brainstorm on interface:
  * should have several ways to specify ratchets
    * one being e.g. `Ratchet.every ~n ~f ~ratchet_e e`
      * where f is of type: ('e0 -> ratchet_v:'e1 -> int -> 'e0 option)
    * another being `Ratchet.Rhythm.index`
      * where the index is relative to rhythm start somehow
*)

let every ~n ~switch_e e =
  e
  |> E.fold (fun i _ -> succ i) (-1)
  |> E.map (fun i -> if i mod n = 0 then switch_e else e)
  |> E.switch e

let every' ~pred ~switch_e ~f e =
  e 
  |> E.fold (fun (i, _) e_v -> succ i, Some e_v) (-1, None)
  |> E.fmap (fun (i, opt_e_v) -> match opt_e_v with
    | None -> None
    | Some e_v -> Some (i, e_v)
  )
  |> E.map (fun (i, v) ->
    if pred ~v ~i then
      switch_e |> E.fold (fun (i_ratchet, _) v_ratchet ->
        let v = f ~v ~i ~v_ratchet ~i_ratchet in
        succ i_ratchet, v
      ) (0, None)
      |> E.fmap (fun (_, opt_e_v) -> opt_e_v)
    else e
  )
  |> E.switch e

