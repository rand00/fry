open Lwt_react

module Beat = Fry.Beat.Make(struct
  let bpm_s = S.const (120. *. 3.)
  let sleep = Lwt_unix.sleep
end)

let rhythm_e =
  let rhythms = Fry.Rhythm.[
    Euclidean.make ~len:5 ~n:4;
    Euclidean.make ~len:5 ~n:3;
    Euclidean.make ~len:8 ~n:7 |> rotate_left 4;
  ]
  in
  Beat.e
  |> E.fmap (fun tick ->
    if CCList.for_all (Fry.Rhythm.Bool.is_on ~tick) rhythms then
      Some tick
    else None
  )

let _out =
  rhythm_e |> E.trace (fun i ->
    Printf.printf "merged polyrhythmic euclidians on tick = %d\n%!" i
  )

let () = Lwt_main.run @@ Beat.run ()
