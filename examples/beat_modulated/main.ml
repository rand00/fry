open Lwt_react 
open Lwt.Infix
open Gg

let render_fps = 60.

let sleep = Lwt_unix.sleep

let render_tick_e =
  let bpm_s = S.const @@ Fry.Time.bpm_of_fps render_fps in
  Fry.Beat.make ~bpm_s

let bpm_sine_s =
  let f =
    let duration = 0.5 (*secs*) in
    let length = duration *. render_fps in
    Fry.Envelope.(sine ~length |> Inf.of_finite ~length)
  in
  (*> Note: A single event that triggers an infinite envelope*)
  Fry.Event.create_instant ~sleep
  |> Fry.Envelope.create ~tick_e:render_tick_e ~f
  |> S.map (fun v -> 10. +. 20. *. v)

let beat_e = Fry.Beat.make ~bpm_s:bpm_sine_s

(*> Note: phase-correction is done as this envelopes length depend on the bpm.
    If there is no phase-correction of a bpm-dependent envelope, it will go
    back and forwards in time - which can be interesting in itself, but is
    probably not what was intended.
  
    In contrast, with phase-correction, the envelope will just
    slow-down/speed-up.
*)
let envelope =
  let phase_correct = Fry.Envelope.make_phase_correct () in
  fun (prev_bpm, bpm) ->
    let calc_length bpm = 
      let duration = Fry.Time.of_bpm bpm /. 1.7 in
      duration *. render_fps
    in
    let length = calc_length bpm in
    let prev_bpm = prev_bpm |> CCOption.get_or ~default:bpm in
    let prev_length = calc_length prev_bpm
    in
    Fry.Envelope.sine ~length
    |> phase_correct ~prev_length ~length

let envelope_s =
  let env_s =
    bpm_sine_s
    |> Fry.Signal.with_prev_value
    |> S.map ~eq:Fry.Eq.never envelope
  in
  beat_e |> Fry.Envelope.of_env_signal ~tick_e:render_tick_e ~f:env_s

let _out = 
  Fry_io.Term.Out.envelopes ~typ:`Line [ envelope_s ]

let () =
  Fry_io.Term.init ();
  let max_bpm = 20000. in
  Lwt_main.run @@ Fry.Beat.run ~sleep ~max_bpm ()



