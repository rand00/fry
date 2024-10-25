open Lwt_react
open Fry_core
open Fry_frp

let bpm = 40.
let bpm_s = S.const bpm

let beat_e = Beat.make ~bpm_s

let render_fps = 60.

let render_tick_e =
  let bpm_s = S.const @@ Time.bpm_of_fps render_fps in
  Beat.make ~bpm_s
  
(*> Note: Here we compose different envelopes to make a complex one*)
let envelope =
  let duration = Time.of_bpm bpm /. 1.5 in
  let length = duration *. render_fps in
  Wavelet.(
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
  beat_e |> Envelope.create ~tick_e:render_tick_e ~f:envelope

let _out =
  Fry_io.Term.Out.envelopes ~typ:`Line [ envelope_s ]

let () =
  Fry_io.Term.init ();
  let sleep = Lwt_unix.sleep in
  let time = Unix.gettimeofday in
  let max_bpm = 20000. in
  Lwt_main.run @@ Beat.run ~sleep ~time ~max_bpm ()


  
