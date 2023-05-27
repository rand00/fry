open Lwt_react

let sample ~at s =
  s |> S.sample (fun _ v -> v) at

let of_signals ?(eq=(=)) : 'a signal list -> 'a list signal = fun signals ->
  let eq = CCList.equal eq in
  let rec aux = function
    | s1 :: s2 :: s3 :: s4 :: s5 :: s6 :: [] -> (* len = 6 *)
      S.l6 (fun v1 v2 v3 v4 v5 v6 -> v1 :: v2 :: v3 :: v4 :: v5 :: v6 :: [])
        s1 s2 s3 s4 s5 s6
    | s1 :: s2 :: s3 :: s4 :: s5 :: [] ->       (* len = 5 *)
      S.l5 (fun v1 v2 v3 v4 v5 -> v1 :: v2 :: v3 :: v4 :: v5 :: [])
        s1 s2 s3 s4 s5 
    | s1 :: s2 :: s3 :: s4 :: s5 :: tl ->       (* len > 5 *)
      S.l6 (fun v1 v2 v3 v4 v5 tl -> v1 :: v2 :: v3 :: v4 :: v5 :: tl)
        s1 s2 s3 s4 s5 (aux tl) (*< Note only recursion*)
    | s1 :: s2 :: s3 :: s4 :: [] ->             (* len = 4 ... *)
      S.l4 (fun v1 v2 v3 v4 -> v1 :: v2 :: v3 :: v4 :: [])
        s1 s2 s3 s4
    | s1 :: s2 :: s3 :: [] ->
      S.l3 (fun v1 v2 v3 -> v1 :: v2 :: v3 :: [])
        s1 s2 s3 
    | s1 :: s2 :: [] ->
      S.l2 (fun v1 v2 -> v1 :: v2 :: [])
        s1 s2 
    | s1 :: [] ->
      S.l1 (fun v1 -> v1 :: [])
        s1 
    | [] -> S.const []
  in
  aux signals 

let with_prev_value s =
  let init = None, S.value s in
  S.changes s
  |> E.fold (fun (_, prev_v) v -> Some prev_v, v) init
  |> S.hold ~eq:Eq.never init

