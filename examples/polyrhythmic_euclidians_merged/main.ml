open Lwt_react

let bpm_s = S.const (120. *. 3.)

let beat_e = Fry.Beat.make ~bpm_s

let rhythm_e =
  let rhythms = Fry.Rhythm.[
    Euclidean.make ~len:5 ~n:4;
    Euclidean.make ~len:5 ~n:3;
    Euclidean.make ~len:8 ~n:7 |> rotate_left 4;
  ]
  in
  beat_e
  |> E.fmap (fun tick ->
    if CCList.for_all (Fry.Rhythm.Bool.is_on ~tick) rhythms then
      Some tick
    else None
  )

let _out =
  rhythm_e |> E.trace (fun i ->
    Printf.printf "merged polyrhythmic euclidians on tick = %d\n%!" i
  )

let () =
  let sleep = Lwt_unix.sleep in
  let max_bpm = 20000. in
  let time = Unix.gettimeofday in
  Lwt_main.run @@ Fry.Beat.run ~sleep ~time ~max_bpm ()
