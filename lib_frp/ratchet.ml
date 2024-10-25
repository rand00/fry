open Lwt_react

let every ~n ~switch_e e =
  e
  |> E.fold (fun i _ -> succ i) (-1)
  |> E.map (fun i -> if i mod n = 0 then switch_e else e)
  |> E.switch e

let choose chooser e =
  e
  |> E.map CCFun.(chooser%>CCOption.get_or ~default:e)
  |> E.switch e

