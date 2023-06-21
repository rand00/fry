open Lwt_react

let bpm = 40.
let bpm_s = S.const bpm

let beat_e = Fry.Beat.make ~bpm_s

let render_fps = 60.

let render_tick_e =
  let bpm_s = S.const @@ Fry.Time.bpm_of_fps render_fps in
  Fry.Beat.make ~bpm_s
  
(*> Note: Here we compose different envelopes to make a complex one*)
let envelope =
  let duration = Fry.Time.of_bpm bpm /. 1.5 in
  let length = duration *. render_fps in
  Fry.Envelope.(
    mul
      (sine ~length)
      (
        let length = length /. 3. in
        sine ~length
        |> Inf.of_finite ~length
        |> cmul 0.5
        |> phase ~length ~shift:(length /. 1.9)
      )
    |> normalize_on_i ~length ~v_static:0
  )

let envelope_s =
  beat_e |> Fry.Envelope.create ~tick_e:render_tick_e ~f:envelope

let _out =
  Fry_io.Term.Out.envelopes ~typ:`Line [ envelope_s ]

let () =
  Fry_io.Term.init ();
  let sleep = Lwt_unix.sleep in
  let max_bpm = 20000. in
  Lwt_main.run @@ Fry.Beat.run ~sleep ~max_bpm ()


  
