open Lwt_react 
open Lwt.Infix
open Gg

let render_fps = 60.

let sleep = Lwt_unix.sleep

module Render_tick = Fry.Beat.Make(struct
    let bpm_s = S.const @@ Fry.Time.bpm_of_fps render_fps
    let sleep = sleep
  end)

let bpm_sine_s =
  let f =
    let duration = 0.5 (*secs*) in
    let length = duration *. render_fps in
    Fry.Envelope.(sine ~length |> Inf.of_finite ~length)
  in
  Fry.Event.make_instant_singleton ~sleep
  |> Fry.Envelope.create ~tick_e:Render_tick.e ~f
  |> S.map (fun v -> 10. +. 6. *. v)

module Beat = Fry.Beat.Make(struct
    let bpm_s = bpm_sine_s
    let sleep = sleep
  end)

(*> Note that this goes back/forwards in time as bpm is modulated*)
let envelope bpm =
  let duration = Fry.Time.of_bpm bpm /. 1.7 in
  let length = duration *. render_fps 
  in
  Fry.Envelope.sine ~length

let envelope_s =
  let env_s = bpm_sine_s |> S.map ~eq:Fry.Eq.never envelope in
  Beat.e |> Fry.Envelope.of_env_signal ~tick_e:Render_tick.e ~f:env_s

let _out = 
  Fry_io.Term.Out.envelopes ~typ:`Line [ envelope_s ]

let () =
  Fry_io.Term.init ();
  Lwt_main.run @@
  Lwt.pick [ Beat.run (); Render_tick.run () ]



