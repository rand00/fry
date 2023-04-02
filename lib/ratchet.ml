open Lwt_react

let every ~n ~switch_e e =
  e
  |> E.fold (fun i _ -> succ i) (-1)
  |> E.map (fun i -> if i mod n = 0 then switch_e else e)
  |> E.switch e

let choose ~on e =
  e 
  |> E.fold (fun (i, _) e_v -> succ i, Some e_v) (-1, None)
  |> E.fmap (fun (i, opt_e_v) -> match opt_e_v with
    | None -> None
    | Some e_v -> Some (i, e_v)
  )
  |> E.map (fun (i, v) ->
    match on ~v ~i with
    | Some (ratchet_e, ratchet_f) ->
      ratchet_e |> E.fold (fun (i_ratchet, _) v_ratchet ->
        let v = ratchet_f ~v_ratchet ~i_ratchet in
        succ i_ratchet, v
      ) (0, None)
      |> E.fmap (fun (_, opt_e_v) -> opt_e_v)
    | None -> e
  )
  |> E.switch e

