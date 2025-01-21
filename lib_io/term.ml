open Lwt_react
open Gg
open Fry_core
open Fry_frp
    
(*goto rename, to what?*)
module Term = Notty_lwt.Term 

let (term_s : Term.t option S.t), term_supd =
  S.create ~eq:Eq.never None

(*> goto put the input/output helpers in separate scopes?*)

let events_e =
  term_s
  |> S.changes
  |> E.fmap (fun term_opt ->
    term_opt |> CCOption.map (fun term ->
      Notty_lwt.Term.events term |> E.of_stream
    )
  )
  |> E.switch (Event.never ())

let mouse_drag_abs_e =
  events_e |> E.fmap (function
    | `Mouse (`Drag, (x, y), mods) ->
      Some (`Drag ((V2.v (float x) (float y), mods)))
    | `Mouse (`Release, _, _) -> Some `Release
    | _ -> None
  )
  (* |> E.trace (fun v2 ->
   *   C.log "DEBUG: mouse drag abs %.2f %.2f" (V2.x v2) (V2.y v2)
   * ) *)

let mouse_drag_rel_e' =
  let init = `Release, None
  in
  mouse_drag_abs_e
  |> E.fold (fun (last_rel, last_abs) -> function
    | `Drag (abs, mods) ->
      begin match last_abs with
        | Some last_abs -> 
          `Drag (V2.(abs - last_abs), mods), Some abs
        | None ->
          `Drag (V2.zero, mods), Some abs
      end
    | `Release -> `Release, None
  ) init
  |> E.map fst
  |> E.map (function
    | `Release as v -> v
    | `Drag (v2, mods) ->
      (*> goto this should also be the case for mouse_drag_abs_e...
        .. but that would depend on height of terminal.. 
      *)
      let inverted_rel = V2.(mul v2 (v 1. (-1.))) in
      `Drag (inverted_rel, mods)
  )
  (* |> E.trace (fun v2 ->
   *   C.log "DEBUG: mouse drag rel %.2f %.2f" (V2.x v2) (V2.y v2)
   * ) *)

let mouse_drag_rel_e =
  mouse_drag_rel_e'
  |> E.map (function
    | `Release -> V2.zero, []
    | `Drag v -> v
  )

let keys_e = 
  events_e |> E.fmap (function
    | `Key key -> Some key
    | _ -> None
  )

(*goto make a Sample module initiated with tick_e?*)
let sample_dimensions ~at =
  let init = 0, 0 in
  let eq = Tuple.eq2 CCInt.equal CCInt.equal in
  let aux _ term = Option.map Term.size term in
  S.sample aux at term_s
  |> E.map (function
    | None -> init
    | Some v -> v
  )
  |> S.hold ~eq init

let render image_e =
  let output_image (image, term) =
    match term with
    | None -> Lwt.return_unit
    | Some term -> Term.image term image
  in
  S.sample (fun v v' -> v, v') image_e term_s
  |> E.map_s output_image

let init () =
  let term = Notty_lwt.Term.create ~mouse:true ~dispose:true () ~nosig:false in
  Lwt_main.at_exit (fun () -> Term.release term);
  term_supd @@ Some term

module Out = struct

  open Notty
  open Gg

  let color_of_env ~high:(r, g, b) ~low:(r', g', b') env =
    let remap = Float.remap ~x0:0. ~x1:1. in
    let r = remap ~y0:(float r') ~y1:(float r) env |> Float.to_int in
    let g = remap ~y0:(float g') ~y1:(float g) env |> Float.to_int in
    let b = remap ~y0:(float b') ~y1:(float b) env |> Float.to_int in
    A.rgb_888 ~r ~g ~b
  
  let box ~w ~h ~high ~low env =
    let row =
      String.make w ' '
      |> I.string A.(bg @@ color_of_env ~high ~low env)
    in
    List.init h (fun _ -> row)
    |> I.vcat

  let line ~w ~h env =
    let y_max = float h *. env |> truncate in
    let row y =
      let is_on = y < y_max in
      String.init w (fun x -> 
        if x = w/2 && is_on then '|' else ' '
      )
      |> I.string A.empty
    in
    List.init h row
    |> I.vcat

  let envelopes ?(typ=`Line) envelopes =
    let envelopes_s = envelopes |> Signal.of_signals ~eq:CCFloat.equal in
    let envelopes_e = envelopes_s |> S.changes in
    let dimensions_s = sample_dimensions ~at:envelopes_e in
    S.l2 ~eq:Eq.never Tuple.mk2
      dimensions_s
      envelopes_s
    |> S.map ~eq:Eq.never (fun ((w, h), envelopes) ->
      let len = List.length envelopes in
      let w_box = float w /. float len |> truncate in
      let mk_image = match typ with
        | `Box -> 
          let low = 31, 33, 46 in
          let high = 77, 83, 117 in 
          box ~w:w_box ~h ~high ~low
        | `Line ->
          line ~w:w_box ~h
      in
      envelopes
      |> List.map mk_image
      |> I.hcat 
    )
    |> S.changes
    |> render

end
