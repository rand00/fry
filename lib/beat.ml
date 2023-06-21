open Lwt_react 
open Lwt.Infix

let fps_s, fps_supd = S.create 333.
    
let (tick_e : int E.t), tick_eupd = E.create ()

module type PARAM = sig
  val bpm_s : float S.t
end

module Make (P : PARAM) = struct 

  let eps = 0.000001

  let e : int E.t =
    let shoot (_out, out_tick, ticks_since_start) (tick, (fps, bpm)) =
      if bpm < eps then
        None, out_tick, ticks_since_start
      else 
        let time_bpm = Time.of_bpm bpm in
        let time_since_start = float ticks_since_start *. (1. /. fps) in
        if time_since_start >= time_bpm then
          let out = Some out_tick in
          let out_tick = succ out_tick in
          let ticks_since_start = 0 in
          out, out_tick, ticks_since_start
        else
          let out = None in
          let ticks_since_start = succ ticks_since_start in
          out, out_tick, ticks_since_start
    in
    S.l2 Tuple.mk2 fps_s P.bpm_s
    |> S.sample Tuple.mk2 tick_e 
    |> E.fold shoot (None, 0, 0)
    |> E.fmap (fun (out, _, _) -> out)
      
  let s = e |> S.hold ~eq:Eq.never 0

end

let run ~sleep ~fps () =
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
  
