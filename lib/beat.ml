open Lwt_react 
open Lwt.Infix

module type PARAM = sig
  val bpm_s : float S.t
  val sleep : float -> unit Lwt.t
end

module Make (P : PARAM) = struct 

  let (e : int E.t), tick_eupd = E.create ()

  let s = e |> S.hold ~eq:Eq.never 0

  let run () =
    let rec aux tick = 
      P.sleep @@ 60. /. S.value P.bpm_s >>= fun () ->
      tick_eupd tick;
      aux @@ succ tick
    in
    aux 0

end

let divide_speed ?(slide=0) ~by tick_e =
  assert (by > 0);
  assert (slide < by);
  tick_e
  |> E.fmap (fun i ->
    if i mod by = slide then Some (i / by) else None
  )
  
