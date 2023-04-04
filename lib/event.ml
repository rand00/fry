open Lwt_react

let add_index e = 
  e |> E.fold (fun acc v -> match acc with
    | None -> Some (0, v)
    | Some (i, _) -> Some (succ i, v)
  ) None
  |> E.fmap CCFun.id

let limit n e =
  add_index e
  |> E.fmap (fun (i, v) ->
    if i < n then Some v else None
  )

