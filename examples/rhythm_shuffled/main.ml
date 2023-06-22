open Lwt_react
open Lwt.Syntax

let bpm = 120. 
let bpm_s = S.const bpm

let beat_e = Fry.Beat.make ~bpm_s

let rhythm = 
  [ false; false; false; true; true; true ] 

let rhythm_e =
  beat_e
  |> E.fold (fun (_, rhythm') tick ->
    if Fry.Rhythm.index ~tick rhythm = 0 then
      let rhythm' = Fry.Rhythm.shuffle rhythm in
      tick, rhythm'
    else
      tick, rhythm'
  ) (-1, rhythm)
  |> E.fmap (fun (tick, rhythm) -> 
    if Fry.Rhythm.Bool.is_on ~tick rhythm then
      Some tick
    else
      None
  )

let _out =
  rhythm_e |> E.trace (fun i ->
    Printf.printf "shuffled rhythm = %d\n%!" i
  )

let () =
  let sleep = Lwt_unix.sleep in
  let max_bpm = 20000. in
  let time = Unix.gettimeofday in
  Lwt_main.run @@ Fry.Beat.run ~sleep ~time ~max_bpm ()
