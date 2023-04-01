open Lwt_react

module Beat = Fry.Beat.Make(struct
    let bpm_s = S.const (120. *. 3.)
  end)

let rhythm_e =
  let rhytms = Fry.Rhythm.[
    Euclidean.make ~len:5 ~n:4;
    Euclidean.make ~len:5 ~n:3;
    Euclidean.make ~len:8 ~n:7 |> rotate_left ~n:4;
  ]
  in
  Beat.e
  |> E.fmap (fun tick ->
    let beat = CCList.for_all (Fry.Rhythm.is_beat ~tick) rhytms in
    if beat then Some tick else None
  )

let _out =
  rhythm_e |> E.trace (fun i ->
    Printf.printf "euclidian rhythms on tick = %d\n%!" i
  )

let () = Lwt_main.run @@ Beat.run ()
