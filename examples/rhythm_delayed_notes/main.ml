open Lwt_react
open Lwt.Syntax
open Fry_core
open Fry_frp

let bpm = 120. 
let bpm_s = S.const bpm

let beat_e = Beat.make ~bpm_s

let rhythm = Rhythm.(
  [ true; true; true; true ] |> Bool.mapi (fun i ->
    if i = 2 then `Delay else `Normal
  )
)

let rhythm_e =
  beat_e
  |> E.fmap (fun tick ->
    Rhythm.Option.get ~tick rhythm |> Option.map (fun v ->
      tick, v
    )
  )
  |> E.map_s (fun (tick, v) -> match v with
    | `Normal -> Lwt.return tick
    | `Delay -> 
      let+ () = Lwt_unix.sleep @@ Time.of_bpm bpm /. 2. in
      tick
  )

let _out =
  rhythm_e |> E.trace (fun i ->
    Printf.printf "4/4 rhythm with delayed 3rd note = %d\n%!" i
  )

let () =
  let sleep = Lwt_unix.sleep in
  let max_bpm = 20000. in
  let time = Unix.gettimeofday in
  Lwt_main.run @@ Beat.run ~sleep ~time ~max_bpm ()
