open Lwt_react

module Beat = Fry.Beat.Make(struct
    let bpm_s = S.const (120. *. 3.)
  end)

let div, slide = 3, 2

let beat'_e = Beat.e |> Fry.Beat.divide_speed ~by:div ~slide

let _out =
  let _ = Beat.e |> E.trace (fun i ->
    Printf.printf "beat: %d\n%!" i
  ) in
  beat'_e |> E.trace (fun i ->
    Printf.printf "beat divided by %d, slided by %d: %d\n%!"
      div slide i
  )

let () = Lwt_main.run @@ Beat.run ()
