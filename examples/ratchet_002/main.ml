open Lwt_react

module Fast_beat = Fry.Beat.Make(struct
    let bpm_s = S.const (120. *. 6.)
  end)

let beat_e = Fast_beat.e |> Fry.Beat.divide_speed ~by:6

let ratchet_e =
  let choose_ratchet (tick, _) =
    if tick mod 4 = 0 then
      let ratchet_e =
        Fast_beat.e
        |> Fry.Event.limit 3
        |> E.map (fun tick' -> tick', "ratchet")
      in
      Some ratchet_e
    else
      None
  in
  beat_e
  |> E.map (fun tick -> tick, "beat")
  |> Fry.Ratchet.choose choose_ratchet

let _out =
  ratchet_e |> E.trace (fun (i, tag) ->
    Printf.printf "%s: %d\n%!" tag i
  )

let () = Lwt_main.run @@ Fast_beat.run ()
