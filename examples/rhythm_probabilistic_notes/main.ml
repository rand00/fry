open Lwt_react
open Lwt.Syntax

let bpm = 120. 
let bpm_s = S.const bpm

let beat_e = Fry.Beat.make ~bpm_s

let rhythm = Fry.Rhythm.(
  [ true; true; true; true ] |> Bool.mapi (function
    | 0 -> `Prob 90.
    | 1 -> `Prob 100.
    | 2 -> `Prob 20.
    | 3 -> `Prob 100.
    | _ -> `Prob 100.
  )
)

let rhythm_e =
  beat_e
  |> E.fmap (fun tick -> CCOption.Infix.(
    Fry.Rhythm.Option.get ~tick rhythm >>= fun (`Prob p) ->
    if Random.float 100. < p then
      Some tick
    else
      None
  ))

let _out =
  rhythm_e |> E.trace (fun i ->
    Printf.printf "probabilistic notes in a rhythm = %d\n%!" i
  )

let () =
  let sleep = Lwt_unix.sleep in
  let max_bpm = 20000. in
  let time = Unix.gettimeofday in
  Lwt_main.run @@ Fry.Beat.run ~sleep ~time ~max_bpm ()
