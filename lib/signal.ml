open Lwt_react

let sample ~at s =
  s |> S.sample (fun _ v -> v) at

