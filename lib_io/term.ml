open Lwt_react
open Gg

(*goto rename, to what?*)
module Term = Notty_lwt.Term 

let (term_s : Term.t option S.t), term_supd =
  S.create ~eq:Fry.Eq.never None

(*> goto put the input/output helpers in separate scopes?*)

let events_e =
  term_s
  |> S.changes
  |> E.fmap (fun term_opt ->
    term_opt |> CCOption.map (fun term ->
      Notty_lwt.Term.events term |> E.of_stream
    )
  )
  |> E.switch (Fry.Event.never ())

let mouse_drag_abs_e =
  events_e |> E.fmap (function
    | `Mouse (`Drag, (x, y), mods) -> Some ((V2.v (float x) (float y), mods))
    | _ -> None
  )
  (* |> E.trace (fun v2 ->
   *   C.log "DEBUG: mouse drag abs %.2f %.2f" (V2.x v2) (V2.y v2)
   * ) *)

let mouse_drag_rel_e =
  let init = ((V2.zero, []), V2.zero)
  in
  mouse_drag_abs_e
  |> E.fold (fun (_last_rel, last_abs) (abs, mods) ->
    (V2.(abs - last_abs), mods), abs
  ) init
  |> E.map fst
  |> E.map (fun (v2, mods) -> V2.(mul v2 (v 1. (-1.))), mods)
  (* |> E.trace (fun v2 ->
   *   C.log "DEBUG: mouse drag rel %.2f %.2f" (V2.x v2) (V2.y v2)
   * ) *)

let keys_e = 
  events_e |> E.fmap (function
    | `Key key -> Some key
    | _ -> None
  )

(*goto make a Sample module initiated with tick_e?*)
let sample_dimensions ~at =
  let init = 0, 0 in
  let aux _ term = Option.map Term.size term in
  S.sample aux at term_s
  |> E.map (function
    | None -> init
    | Some v -> v
  )
  |> S.hold init

let render image_e =
  let output_image (image, term) =
    match term with
    | None -> Lwt.return_unit
    | Some term -> Term.image term image
  in
  S.sample (fun v v' -> v, v') image_e term_s
  |> E.map_s output_image

let init () =
  let term = Term.create ~nosig:false () in
  Lwt_main.at_exit (fun () -> Term.release term);
  term_supd @@ Some term

