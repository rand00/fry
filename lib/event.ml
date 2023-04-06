open Lwt_react

let add_index e = 
  e |> E.fold (fun acc v -> match acc with
    | None -> Some (0, v)
    | Some (i, _) -> Some (succ i, v)
  ) None
  |> E.fmap CCFun.id

let limit n e =
  add_index e
  |> E.fmap (fun (i, v) ->
    if i < n then Some v else None
  )

let reccur ~at init e = S.hold init e |> Signal.sample ~at

let bind ~init f e = e |> E.map f |> E.switch init

let never () = E.create () |> fst

let downsample ~to_ ~zero ~add input_e =
  let accumulate_between_frames
      (acc_v, prev_was_frame, frame, last_frame, frame_of_last_value)
    = function
      | `Frame f ->
        acc_v, true, Some f, frame, frame_of_last_value
      | `Value v ->
        let acc_v = if prev_was_frame then zero else acc_v in
        add acc_v v, false, frame, last_frame, frame
      | `FrameValue (f, v) ->
        let acc_v = if prev_was_frame then zero else acc_v in
        add acc_v v, true, Some f, frame, frame
  in
  let merge acc e = 
    match acc, e with
    | None, e -> Some e
    | Some (`Frame f), `Value v 
    | Some (`Value v), `Frame f -> Some (`FrameValue (f, v))
    | _ -> acc
  in
  E.merge merge None [ 
    to_ |> E.map (fun f -> `Frame f);
    input_e |> E.map (fun v -> `Value v);
  ]
  |> E.fmap CCFun.id
  |> E.fold accumulate_between_frames (zero, true, None, None, None)
  |> E.fmap (
    fun (input_v, is_frame, frame, last_frame, frame_of_last_value) ->
      if is_frame &&
         CCOption.equal CCInt.equal
           last_frame
           frame_of_last_value
      then Some input_v
      else None
  )

