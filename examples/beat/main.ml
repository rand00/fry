open Lwt_react

let bpm_s = S.const 120.

let beat_e = Fry_frp.Beat.make ~bpm_s

let div, slide = 3, 2

let beat'_e = beat_e |> Fry_frp.Beat.divide_speed ~by:div ~slide

let _out =
  beat_e |> E.trace (fun i ->
    Printf.printf "beat: %d\n%!" i
  ) |> E.keep;
  beat'_e |> E.trace (fun i ->
    Printf.printf "beat divided by %d, slided by %d: %d\n%!"
      div slide i
  ) |> E.keep

let () =
  let sleep = Lwt_unix.sleep in
  let max_bpm = 20000. in
  let time = Unix.gettimeofday in
  Lwt_main.run @@ Fry_frp.Beat.run ~sleep ~time ~max_bpm ()


