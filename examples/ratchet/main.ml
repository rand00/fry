open Lwt_react

module Beat = Fry.Beat.Make(struct
    let bpm_s = S.const (120. *. 6.)
  end)

let beat'_e = Beat.e |> Fry.Beat.divide_speed ~by:6

let ratchet_e = beat'_e |> Fry.Ratchet.every ~n:4 ~switch_e:Beat.e

let _out =
  ratchet_e |> E.trace (fun i ->
    Printf.printf "ratchet: %d\n%!" i
  )

let () = Lwt_main.run @@ Beat.run ()
