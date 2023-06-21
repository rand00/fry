open Lwt_react 
open Lwt.Infix

let fps_s, fps_supd = S.create 333.
    
let (tick_e : int E.t), tick_eupd = E.create ()

let make ~bpm_s = 
  let eps = 0.000001 in
  let shoot (_out, out_tick, ticks_since_beat) (tick, (fps, bpm)) =
    if bpm < eps then
      None, out_tick, ticks_since_beat
    else 
      let time_bpm = Time.of_bpm bpm in
      let time_since_start = float ticks_since_beat *. (1. /. fps) in
      if time_since_start >= time_bpm then
        let out = Some out_tick in
        let out_tick = succ out_tick in
        let ticks_since_beat = 0 in
        out, out_tick, ticks_since_beat
      else
        let out = None in
        let ticks_since_beat = succ ticks_since_beat in
        out, out_tick, ticks_since_beat
  in
  S.l2 Tuple.mk2 fps_s bpm_s
  |> S.sample Tuple.mk2 tick_e 
  |> E.fold shoot (None, 0, 0)
  |> E.fmap (fun (out, _, _) -> out)

let run ~sleep ~max_bpm () =
  let fps = Time.fps_of_bpm max_bpm in
  let rec loop tick =
    tick_eupd tick;
    fps_supd fps;
    sleep (1. /. fps) >>= fun () -> 
    loop @@ succ tick
  in
  loop 0

let divide_speed ?(slide=0) ~by tick_e =
  assert (by > 0);
  assert (slide < by);
  tick_e
  |> E.fmap (fun i ->
    if i mod by = slide then Some (i / by) else None
  )
  
