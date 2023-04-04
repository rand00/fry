open Lwt_react
open Lwt.Syntax

let bpm = 120. 

module Beat = Fry.Beat.Make(struct
  let bpm_s = S.const bpm
  let sleep = Lwt_unix.sleep
end)

let rhythm = Fry.Rhythm.(
  [ true; true; true; true ] |> Bool.mapi (fun i ->
    if i = 2 then `Delay else `Normal
  )
)

let rhythm_e =
  Beat.e
  |> E.fmap (fun tick ->
    Fry.Rhythm.Option.get ~tick rhythm |> Option.map (fun v ->
      tick, v
    )
  )
  |> E.map_s (fun (tick, v) -> match v with
    | `Normal -> Lwt.return tick
    | `Delay -> 
      let+ () = Lwt_unix.sleep @@ Fry.Time.of_bpm bpm /. 2. in
      tick
  )

let _out =
  rhythm_e |> E.trace (fun i ->
    Printf.printf "4/4 rhythm with delayed 3rd note = %d\n%!" i
  )

let () = Lwt_main.run @@ Beat.run ()
