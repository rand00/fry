open Lwt_react
open Lwt.Syntax

type note = {
  delay : float option;
  ratchet : bool;
  (* colour : unit; *) (*< goto come up with something that makes sense for notty*)
}

let bpm_mul = 6.
let bpm = 120. *. bpm_mul

module Fast_beat = Fry.Beat.Make(struct
    let bpm_s = S.const bpm
  end)

let half_fast_beat =
  Fast_beat.e
  |> Fry.Beat.divide_speed ~by:(truncate bpm_mul / 2) 

let normal_beat =
  Fast_beat.e
  |> Fry.Beat.divide_speed ~by:(truncate bpm_mul) 

let x, o = true, false

let rhythm_01 = [ x; o; o; x; x ]
let rhythm_02 = [ x; o; x; o; x ]

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

let eval_rhythm rhythm =
  let ratchet ~v ~i =
    if not v.ratchet then None else (
      if i mod 2 = 0 then
        (*> goto brian if this interface is wanted..
            it depends on event being created here :/
            .. though it's much simpler than current 'choose' interface
               .. and it's much more powerful than previous every' 
        *)
        let ratchet_e = Fast_beat.e |> Fry.Event.limit 4 in
        Some ratchet_e
      else
        let ratchet_e = half_fast_beat |> Fry.Event.limit 2 in
        Some ratchet_e
    )
  in
  (* let ratchet ~v ~i = *)
  (*   if not v.ratchet then None else ( *)
  (*     if i mod 2 = 0 then *)
  (*       let f ~v_ratchet ~i_ratchet = *)
  (*         if i_ratchet < 4 then Some v else None *)
  (*       in *)
  (*       Some (Fast_beat.e, f) *)
  (*     else *)
  (*       let f ~v_ratchet ~i_ratchet = *)
  (*         if i_ratchet < 2 then Some v else None *)
  (*       in *)
  (*       Some (half_fast_beat, f) *)
  (*   ) *)
  (* in *)
  normal_beat
  |> E.fmap (fun tick -> Fry.Rhythm.Option.get ~tick rhythm)
  |> E.map_s (fun v -> match v.delay with
    | None -> Lwt.return v
    | Some d -> let+ () = Lwt_unix.sleep d in v
  )
  |> Fry.Ratchet.choose ~f:ratchet

let rhythm_01_e = eval_rhythm rhythm_01
let rhythm_02_e = eval_rhythm rhythm_02

let _out =
  rhythm_01_e |> E.trace (fun _ -> Printf.printf "rhythm_01 note\n%!") |> E.keep;
  rhythm_02_e |> E.trace (fun _ -> Printf.printf "rhythm_02 note\n%!") |> E.keep

let () = Lwt_main.run @@ Fast_beat.run ()
