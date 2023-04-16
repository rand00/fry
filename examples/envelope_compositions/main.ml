open Lwt_react
open Lwt.Syntax
open Fry.Rhythm.T

let bpm = 40.

module Beat = Fry.Beat.Make(struct
  let bpm_s = S.const bpm
  let sleep = Lwt_unix.sleep
end)

let render_fps = 60.

module Render_tick = Fry.Beat.Make(struct
  let bpm_s = S.const @@ Fry.Time.bpm_of_fps render_fps
  let sleep = Lwt_unix.sleep
end)

(*> Note: Here we compose different envelopes to make a complex one*)
let envelope =
  let duration = Fry.Time.of_bpm bpm /. 1.5 in
  let length = duration *. render_fps in
  Fry.Envelope.(
    mul
      (sine ~length)
      (
        let length = length /. 3. in
        Inf.of_finite ~length (sine ~length)
        |> cmul 0.5
        |> phase ~length ~shift:(length /. 1.9)
      )
    |> normalize_on_i ~length ~v_static:0
  )

let envelope_s =
  Beat.e |> Fry.Envelope.create ~tick_e:Render_tick.e ~f:envelope

let _out =
  Fry_io.Term.Out.envelopes ~typ:`Line ~tick_e:Render_tick.e
    [ envelope_s ]

let () =
  Fry_io.Term.init ();
  Lwt_main.run @@ Lwt.pick [
    Beat.run ();
    Render_tick.run ();
  ]

  
