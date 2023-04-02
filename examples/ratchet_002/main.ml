open Lwt_react

module Fast_beat = Fry.Beat.Make(struct
    let bpm_s = S.const (120. *. 6.)
  end)

let beat_e = Fast_beat.e |> Fry.Beat.divide_speed ~by:6

let ratchet_e =
  let ratchet ~v ~i =
    let f ~v_ratchet ~i_ratchet =
      if i_ratchet < 3 then Some (v_ratchet, "ratchet") else None
    in
    if i mod 4 = 0 then
      Some (Fast_beat.e, f)
    else
      None
  in
  beat_e
  |> E.map (fun v -> v, "beat")
  |> Fry.Ratchet.choose ~on:ratchet

let _out =
  ratchet_e |> E.trace (fun (i, tag) ->
    Printf.printf "%s: %d\n%!" tag i
  )

let () = Lwt_main.run @@ Fast_beat.run ()
