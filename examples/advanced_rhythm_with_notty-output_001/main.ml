open Lwt_react
open Lwt.Syntax

open Fry.Rhythm.T

type note = {
  delay : float option;
  ratchet : bool;
  (* colour : unit; *) (*< goto come up with something that makes sense for notty*)
}

(*> Note the divisibility of 2; for sync of beats when halving them*)
let bpm_mul = 2. ** 3. 
let bpm = 120. *. bpm_mul

module Fast_beat = Fry.Beat.Make(struct
  let bpm_s = S.const bpm
  let sleep = Lwt_unix.sleep
end)

let half_fast_beat =
  Fast_beat.e
  |> Fry.Beat.divide_speed ~by:(truncate bpm_mul / 2) 

let normal_beat =
  Fast_beat.e
  |> Fry.Beat.divide_speed ~by:(truncate bpm_mul) 

(*> Note: we use these aliases for true/false to easily visualize the
    parallel rhythms in OCaml syntax*)
let x, o = true, false

let rhythm_01 = [ x; o; o; x; x ]
let rhythm_02 = [ x; o; x; o; x; o ]

let rhythm_01 =
  rhythm_01 |> Fry.Rhythm.Bool.mapi (function
    | 0 -> { delay = None; ratchet = false }
    | 1 -> { delay = None; ratchet = true }
    | _ ->
      let delay = Option.some @@ Fry.Time.of_bpm bpm /. 2. in
      { delay; ratchet = false }
  )

let rhythm_02 =
  rhythm_02 |> Fry.Rhythm.Bool.mapi (function
    | 0 ->
      let delay = Option.some @@ Fry.Time.of_bpm bpm /. 2. in
      { delay; ratchet = false }
    | 1 -> { delay = None; ratchet = false }
    | _ -> { delay = None; ratchet = false }
  )

(*> Note that this function returns dynamically created events which
    are passed to `Fry.Event.limit`*)
let choose_ratchet v =
  let stamp_note e =
    E.stamp e { v with note = { delay = None; ratchet = false }}
  in
  if not v.note.ratchet then None else (
    (*> Note the use of the Fry.Rhythm.context, which contains rhythm_index*)
    if v.rhythm_index mod 2 = 0 then
      let ratchet_e =
        Fast_beat.e |> Fry.Event.limit 4 |> stamp_note
      in
      Some ratchet_e
    else
      let ratchet_e =
        half_fast_beat |> Fry.Event.limit 2 |> stamp_note
      in
      Some ratchet_e
  )

(*> Note: A general evaluation function for rhythms of the same type*)
let eval_rhythm rhythm =
  normal_beat
  |> E.fmap (fun tick -> Fry.Rhythm.Option.get_with_context ~tick rhythm)
  |> E.map_s (fun v -> match v.note.delay with
    | None -> Lwt.return v
    | Some d -> let+ () = Lwt_unix.sleep d in v
  )
  |> Fry.Ratchet.choose choose_ratchet

let rhythm_01_e = eval_rhythm rhythm_01
let rhythm_02_e = eval_rhythm rhythm_02

let render_fps = 30.

(*> Note: This is for defining envelopes over the rhythms*)
module Render_tick = Fry.Beat.Make(struct
  let bpm_s = S.const (render_fps *. 60. (*secs*))
  let sleep = Lwt_unix.sleep
end)

let env_duration = 0.2
let env_length = env_duration *. render_fps

(*> Note: For each rhythmic beat, a smooth envelope is created*)
let envelope_01_s =
  rhythm_01_e |> Fry.Envelope.create ~tick_e:Render_tick.e
    ~f:(Fry.Envelope.sine ~length:env_length)

let envelope_02_s =
  rhythm_02_e |> Fry.Envelope.create ~tick_e:Render_tick.e
    ~f:(Fry.Envelope.sine ~length:env_length)

let _out =
  Fry_io.Term.Out.envelopes ~typ:`Box
    [ envelope_01_s;
      envelope_02_s ]

let () =
  Fry_io.Term.init ();
  Lwt_main.run @@ Lwt.pick [
    Fast_beat.run ();
    Render_tick.run ();
  ]
