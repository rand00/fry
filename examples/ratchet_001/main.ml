open Lwt_react

let bpm_s = S.const (120. *. 6.)

let beat_e = Fry.Beat.make ~bpm_s

let beat'_e = beat_e |> Fry.Beat.divide_speed ~by:6

let ratchet_e = beat'_e |> Fry.Ratchet.every ~n:4 ~switch_e:beat_e

let _out =
  ratchet_e |> E.trace (fun i ->
    Printf.printf "ratchet: %d\n%!" i
  )

let () =
  let sleep = Lwt_unix.sleep in
  let max_bpm = 20000. in
  Lwt_main.run @@ Fry.Beat.run ~sleep ~max_bpm ()
