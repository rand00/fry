open Lwt_react
open Lwt.Syntax

let bpm = 120. 

module Beat = Fry.Beat.Make(struct
  let bpm_s = S.const bpm
  let sleep = Lwt_unix.sleep
end)

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
  Beat.e
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

let () = Lwt_main.run @@ Beat.run ()
