open Lwt_react 
open Lwt.Infix

(*> goto don't expose fps_supd*)
let fps_s, fps_supd = S.create 333.
    
(*> goto don't expose tick_eupd*)
let (tick_e : int E.t), tick_eupd = E.create ()

let make ~bpm_s = 
  let eps = 0.000001 in
  let shoot (_out, out_tick, ticks_since_beat, diff_time) (tick, (fps, bpm)) =
    if bpm < eps then
      None, out_tick, ticks_since_beat, 0.
    else 
      let time_bpm = Time.of_bpm bpm in
      let ticks_since_beat = succ ticks_since_beat in
      let time_since_beat = float ticks_since_beat *. (1. /. fps) in
      let time_since_beat_adjusted = time_since_beat +. diff_time in
      if time_since_beat_adjusted >= time_bpm then
        let out = Some out_tick in
        let out_tick = succ out_tick in
        let ticks_since_beat = 0 in
        let diff_time = time_since_beat_adjusted -. time_bpm in
        out, out_tick, ticks_since_beat, diff_time
      else
        let out = None in
        out, out_tick, ticks_since_beat, diff_time
  in
  S.l2 ~eq:Eq.never Tuple.mk2 fps_s bpm_s
  |> S.sample Tuple.mk2 tick_e 
  |> E.fold shoot (None, 0, 0, 0.)
  |> E.fmap (fun (out, _, _, _) -> out)

let run ~sleep ~time ~max_bpm () =
  let fps = Time.fps_of_bpm max_bpm in
  fps_supd fps;
  let perfect_sleep_time = 1. /. fps in
  let start = time () in
  let rec loop tick =
    tick_eupd tick;
    let perfect_time = start +. float tick *. Time.of_bpm max_bpm in
    let now = time () in
    let diff_time = perfect_time -. now in
    let sleep_time =
      CCFloat.max 0. (perfect_sleep_time +. diff_time)
    in
    sleep sleep_time >>= fun () -> 
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
  
