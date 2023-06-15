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

  (*> Note that modulating bpm will probably not accumulate to a precise bpm*)
  (*gomaybe there could be some potential to implement using S.bind, could try again
    ideas;
    * trigger/sample first bpm-value, if using S.changes
  *)
  (*> goto goo; when slowing down bpm, the existing beat should be
      extended instead of letting it shoot at its planned time
      * this should happen recursively, so last time a beat was started and
        wasn't resolved is kept in acc state
  *)
  let rec choose_bpm_and_sleep bpm =
    let sleep_t =
      if bpm < eps then
        let never_t = Lwt_mvar.(create_empty () |> take) in
        (*< Note: created here to be repetitively cancelable*)
        never_t
      else
        P.sleep @@ 60. /. bpm
    in
    let new_bpm_t = slow_sample_bpm bpm in
    Lwt.pick [
      (*> goto this should never resolve if new bpm is received*)
      sleep_t;
      new_bpm_t >>= choose_bpm_and_sleep;
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
  
