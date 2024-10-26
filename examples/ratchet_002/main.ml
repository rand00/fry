open Lwt_react
open Fry_frp

let bpm_s = S.const (120. *. 6.)

let fast_beat_e = Beat.make ~bpm_s

let beat_e = fast_beat_e |> Beat.divide_speed ~by:6

let ratchet_e =
  let choose_ratchet (tick, _) =
    if tick mod 4 = 0 then
      let ratchet_e =
        fast_beat_e
        |> Event.limit 3
        |> E.map (fun tick' -> tick', "ratchet")
      in
      Some ratchet_e
    else
      None
  in
  beat_e
  |> E.map (fun tick -> tick, "beat")
  |> Ratchet.choose choose_ratchet

let _out =
  ratchet_e |> E.trace (fun (i, tag) ->
    Printf.printf "%s: %d\n%!" tag i
  )

let () =
  let sleep = Lwt_unix.sleep in
  let max_bpm = 20000. in
  let time = Unix.gettimeofday in
  Lwt_main.run @@ Beat.run ~sleep ~time ~max_bpm ()
