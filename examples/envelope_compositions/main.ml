open Lwt_react
open Lwt.Syntax
open Fry.Rhythm.T

module Beat = Fry.Beat.Make(struct
  let bpm_s = S.const 120.
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
  let duration = 0.15 in
  let length = duration *. render_fps in
  sum [
    (sine ~length
     |> cut_start (length /. 2.)
     (* |> cut_end (length /. 2.) *)
    );
    (sine ~length:(length /. 2.) |> cmul 2.);
    (
      let x = length /. 4. in
      adsr (x*.2., 1.) (x/.2., 1.) (x/.2., 1.) (x*.2., 0.)
    );
  ]
  |> normalize_on_i ~length ~v_static:0
  (* |> trace "adsr" *)

(*goto try making a dynamic envelope wrapper that has pure env branches
  .. which can therefore be normalized individually
    .. and match on `Ratchet vs `Beat for choosing envelope
*)
let envelope_s =
  Beat.e
  |> Fry.Envelope.create ~tick_e:Render_tick.e ~f:envelope

module Out = struct

  let dimensions_s = Fry_io.Term.sample_dimensions ~at:Render_tick.e

  open Notty
  open Gg

  let color_of_env ~high:(r, g, b) ~low:(r', g', b') env =
    let remap = Float.remap ~x0:0. ~x1:1. in
    let r = remap ~y0:(float r') ~y1:(float r) env |> Float.to_int in
    let g = remap ~y0:(float g') ~y1:(float g) env |> Float.to_int in
    let b = remap ~y0:(float b') ~y1:(float b) env |> Float.to_int in
    A.rgb_888 ~r ~g ~b
  
  let box ~w ~h ~high ~low env =
    let bg_line =
      String.make w ' '
      |> I.string A.(bg @@ color_of_env ~high ~low env)
    in
    List.init h (fun _ -> bg_line)
    |> I.vcat
  
  let image_e =
    S.l2 ~eq:Fry.Eq.never Fry.Tuple.mk2
      dimensions_s
      envelope_s
    |> S.map ~eq:Fry.Eq.never (fun ((w, h), env) ->
      let low = 31, 33, 46 in
      let high = 77, 83, 117 in 
      let box = box ~h ~high ~low in
      I.(box ~w env)
    )
    |> S.changes

  let _out = Fry_io.Term.render image_e
  
end

let () =
  Fry_io.Term.init ();
  Lwt_main.run @@ Render_tick.run ()

  
