open Lwt_react
open Fry_frp

let bpm_s = S.const (120. *. 6.)

let beat_e = Beat.make ~bpm_s

let beat'_e = beat_e |> Beat.divide_speed ~by:6

let ratchet_e = beat'_e |> Ratchet.every ~n:4 ~switch_e:beat_e

let _out =
  ratchet_e |> E.trace (fun i ->
    Printf.printf "ratchet: %d\n%!" i
  )

let () =
  let sleep = Lwt_unix.sleep in
  let max_bpm = 20000. in
  let time = Unix.gettimeofday in
  Lwt_main.run @@ Beat.run ~sleep ~time ~max_bpm ()
