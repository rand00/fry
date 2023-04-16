open Lwt_react
open Lwt.Syntax
open Fry.Rhythm.T

let bpm = 30.

module Beat = Fry.Beat.Make(struct
  let bpm_s = S.const bpm
  let sleep = Lwt_unix.sleep
end)

let render_fps = 60.

module Render_tick = Fry.Beat.Make(struct
  let bpm_s = S.const @@ Fry.Time.bpm_of_fps render_fps
  let sleep = Lwt_unix.sleep
end)

(*> Note: Here we compose different envelopes to make a more complex one*)
let envelope =
  let open Fry.Envelope in
  let duration = (Fry.Time.of_bpm bpm) /. 1.0 in
  let length = duration *. render_fps in
  sum [
    sine ~length;
    (* ( *)
    (*   let length = length /. 5. in *)
    (*   Inf.of_finite ~length (sine ~length) *)
    (*   |> cmul 0.3 *)
    (* ); *)
    (* ( *)
    (*   let x = length /. 4. in *)
    (*   adsr (x*.2., 0.8) (x/.2., 1.) (x/.2., 0.8) (x*.2., 0.) *)
    (* ); *)
  ]
  |> normalize_on_i ~length ~v_static:0
  |> trace "adsr"

(*goto try making a dynamic envelope wrapper that has pure env branches
  .. which can therefore be normalized individually
    .. and match on `Ratchet vs `Beat for choosing envelope
*)
let envelope_s =
  Beat.e |> Fry.Envelope.create ~tick_e:Render_tick.e ~f:envelope

let _out =
  Fry_io.Term.Out.envelopes ~tick_e:Render_tick.e
    [ envelope_s ]

let () =
  Fry_io.Term.init ();
  Lwt_main.run @@ Lwt.pick [
    Beat.run ();
    Render_tick.run ();
  ]

  
