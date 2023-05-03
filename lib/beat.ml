open Lwt_react 
open Lwt.Infix

module type PARAM = sig
  val bpm_s : float S.t
  val sleep : float -> unit Lwt.t
end

module Make (P : PARAM) = struct 

  let (e : int E.t), tick_eupd = E.create ()

  let s = e |> S.hold ~eq:Eq.never 0

  (*Note: needed this custom version 'Lwt_react.next' as it shouldn't cancel event*)
  let e_next ev =
    let waiter, wakener = Lwt.task () in
    let ev = E.map (fun x -> Lwt.wakeup wakener x) (E.once ev) in
    waiter

  let bpm_e = S.changes P.bpm_s

  let rec slow_sample_bpm prev =
    P.sleep (1./.20.) >>= fun () ->
    let bpm = S.value P.bpm_s in
    if prev = bpm then
      slow_sample_bpm prev
    else
      Lwt.return bpm
  
  let rec choose_bpm_and_sleep bpm =
    Lwt.pick [
      P.sleep @@ 60. /. bpm;
      slow_sample_bpm bpm >>= choose_bpm_and_sleep;
    ]

  let run () =
    let rec loop tick =
      choose_bpm_and_sleep (S.value P.bpm_s) >>= fun () ->
      tick_eupd tick;
      loop @@ succ tick
    in
    loop 0

end

let divide_speed ?(slide=0) ~by tick_e =
  assert (by > 0);
  assert (slide < by);
  tick_e
  |> E.fmap (fun i ->
    if i mod by = slide then Some (i / by) else None
  )
  
