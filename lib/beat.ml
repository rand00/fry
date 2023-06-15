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
  (* let e_next ev = *)
  (*   let waiter, wakener = Lwt.task () in *)
  (*   let ev = E.map (fun x -> Lwt.wakeup wakener x) (E.once ev) in *)
  (*   waiter *)

  (* let bpm_e = S.changes P.bpm_s *)

  (*> Note; I think I did it this way to avoid GC when using S.bind*)
  let rec slow_sample_bpm prev =
    P.sleep (1./.60.) >>= fun () ->
    let bpm = S.value P.bpm_s in
    if prev = bpm then
      slow_sample_bpm prev
    else
      Lwt.return bpm

  let eps = 0.000001

  let never_t = Lwt_mvar.(create_empty () |> take)
    
  (*> Note that modulating bpm will probably not accumulate to a precise bpm*)
  (*gomaybe there could be some potential to implement using S.bind, could try again
    ideas;
    * trigger/sample first bpm-value, if using S.changes
  *)
  let rec choose_bpm_and_sleep bpm =
    let sleep_t =
      if bpm < eps then never_t else
        P.sleep @@ 60. /. bpm
    in
    Lwt.pick [
      sleep_t;
      slow_sample_bpm bpm >>= choose_bpm_and_sleep;
    ]

  let run () =
    let rec loop tick =
      tick_eupd tick;
      choose_bpm_and_sleep (S.value P.bpm_s) >>= fun () ->
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
  
