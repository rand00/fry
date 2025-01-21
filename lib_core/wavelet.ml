
let pure x ~i ~v = x
let const = pure

let clamp_float (from:float) (to_:float) (v:float) =
  if v < from then from
  else if v > to_ then to_
  else v

let to_int f = f |> CCFloat.round |> CCFloat.to_int

module Inf = struct

  let of_finite ?on_repeat ~length f ~i ~v =
    let length = to_int length in
    let i = i mod length in
    match on_repeat with
    | None -> f ~i ~v
    | Some g ->
      let i_repeat = i / length in
      g ~i_repeat ~i @@ f ~i ~v

  let keep_last_value ~length f ~i ~v =
    let length = to_int length in
    if i >= length then f ~i:(pred length) ~v else f ~i ~v

end

(*spec for envelope functions:
  * they should all be finite by default
  * when an envelope ends, its value goes to 0.
*)

let sine ~length ~i ~v =
  let i_f = float i in
  let i_pct = i_f /. length in
  if i_pct >= 1. then 0. else 
    let angle = i_pct *. CCFloat.pi *. 2. in
    (1. +. sin (angle -. CCFloat.pi /. 2.)) /. 2.

let drop length f ~i ~v =
  f ~i:(to_int length + i) ~v

let take length f ~i ~v =
  if i >= to_int length then 0. else f ~i ~v

let zero ~i ~v = 0.
let one ~i ~v = 1.

let apply op f g ~i ~v = op (f ~i ~v) (g ~i ~v)

let add f g = apply (+.) f g
(*> Warning: the argument-ordering is made intuitive for piping, not normal application*)
let sub g f = apply (-.) f g
let mul f g = apply ( *. ) f g

let and_ = mul
(*> Note:
  * warning: the argument-ordering is made intuitive for piping, not normal application
  * warning: the continuity of the output of this depends on the given
    envelopes
    * e.g.
      * if the left envelope goes to 0. before the end, then it will jump
        to the right envelopes value
      * if the right envelope does not start at the same value as the first
        envelope ends at (before 0.) - then there is a jump
    * though this semantics also enables interesting switch-semantics
      e.g. by multiplying left envelope by a 'boolean' filter-envelope
*)
let or_ ?(eps=0.0000001) g f =
  let aux fv gv = if fv > eps then fv else gv in
  apply aux f g

let max f g = apply CCFloat.max f g
let min f g = apply CCFloat.min f g

let gt f g = apply (fun (x:float) y -> if x > y then 1. else 0.) f g 
let lt f g = gt g f
let eq ~eps f g =
  let eps' = eps /. 2. in
  apply (fun x y ->
    if x +. eps' > y && x -. eps' < y then 1. else 0.
  ) f g 

(*> Note: useful for composition over time*)
let delay ~n f ~i ~v =
  let n = to_int n in
  if i < n then
    0.
  else (*i >= n*)
    let i = i - n in
    f ~i ~v

(*> goto make ranges typesafe (unit-types) somehow?*)
(*spec ranges param: a list of absolute frame-index tuples: start * stop *)
let extract ~ranges anim = 
  ranges |> CCList.fold_left (fun (acc_anim, acc_start) (start, stop) ->
    let length = stop -. start in
    let anim_snippet = anim |> drop start |> take length in
    let acc_anim =
      acc_anim |> add (
        delay ~n:(float acc_start) anim_snippet
      )
    in
    let acc_start = acc_start + truncate length in
    acc_anim, acc_start
  ) (zero, 0)
  |> fst

(*> goto test; hasn't been used, but is derived from another more custom helper
    I made outside of Fry *)
let random_chronological_switch ~seed ~length anims =
  let module R = Random.State in
  let r = R.make [| seed |] in
  let len_anims = CCArray.length anims in
  let anim_idxs =
    let rec aux ~prev_idx = function
      | 0 -> []
      | n -> 
        let idx =
          (*> goto enable passing this pct as param*)
          if R.float r 100. > 90. then
            let idx = R.int r len_anims in
            if idx = prev_idx then
              succ idx mod len_anims
            else idx
          else
            prev_idx
        in
        idx :: aux ~prev_idx:idx (pred n)
    in
    aux ~prev_idx:0 length
    (* |> CCList.rev  *)
    |> CCArray.of_list
  in
  fun ~i ~v ->
    if i >= length then 0. else 
      let idx = anim_idxs.(i) in
      let anim = anims.(idx) in
      anim ~i ~v

(*> goto problem: this makes 'f' into an infinite envelope...
  * problem if casing on value of 'i':
    * either the function won't support finite envelopes,
    * or it wont support infinite ones
    * @idea; could check if 'f ~i ~v > 0.' and then expect it to be infinite if
      also 'i >= length'
    * @idea (better); could also make two versions - one for finite and one infinite
*)
(*> goto support negative shifting*)
let phase ~length ~shift f ~i ~v =
  (*> Note: the reason for 'mod length' is that envelopes can be finite*)
  let i = (i + to_int shift) mod to_int length in
  f ~i ~v

(*> Note that modulating length will probably not accumulate to a precise
    envelope position relative to modulated bpm.
    Therefore you should probably keep the envelope length quite a lot shorter
    than the full length between beats, to avoid cutting off envelope midway
*)
let make_phase_correct () =
  let acc_diff_i = ref 0 in
  fun ~prev_length ~length f ~i ~v -> 
    let prev_length' = to_int prev_length in
    if prev_length' > 0 then (
      if i = 0 then
        acc_diff_i := 0
      else 
        let prev_env_i_pct = float i /. prev_length in
        let i' = prev_env_i_pct *. length |> to_int in
        let diff_i = i' - i in
        acc_diff_i := !acc_diff_i + diff_i;
        (* Printf.eprintf "phase-correct: i = %d, \ *)
        (*                 prev_length = %.2f, length = %.2f, \ *)
        (*                 prev_env_i_pct = %.2f, i' = %d, \ *)
        (*                 diff_i = %d, acc_diff_i = %d \n%!" *)
        (*   i prev_length length *)
        (*   prev_env_i_pct i' *)
        (*   diff_i !acc_diff_i *)
        (* ; *)
    );
    let i = i + !acc_diff_i in
    f ~i ~v

let sum fs ~i ~v = CCList.fold_left (fun acc f -> acc +. f ~i ~v) 0. fs

let cmul c f = mul f @@ pure c
let cadd c f = add f @@ pure c
let csub c f = sub (pure c) f
(*< Note subtraction is not commutative, and 'sub' is already made for piping,
    of operators, which means opposite argument-order *)

let cmin c f = min f @@ pure c
let cmax c f = max f @@ pure c

(** Note that this depends on 'f' being pure*)
let normalize_on_i ?(cut_negative=true) ~length ~v_static f =
  let vs = CCList.init (to_int length) (fun i -> f ~i ~v:v_static) in
  let min_v, max_v = vs |> CCList.fold_left (fun (min_v, max_v) v ->
    CCFloat.min min_v v, CCFloat.max max_v v
  ) (Float.max_float, CCFloat.min_value)
  in
  let range_v = max_v -. min_v in
  if min_v >= 0. then
    (*We don't move envelopes that don't hit 0. down*)
    fun ~i ~v -> f ~i ~v /. max_v
  else (
    if cut_negative then
      (*For understandable handling of messy envelopes*)
      fun ~i ~v -> CCFloat.max 0. (f ~i ~v) /. max_v
    else 
      (*If envelope is negative, we move envelope up*)
      fun ~i ~v -> (f ~i ~v -. min_v) /. range_v
  )

(*> Note
  * Semantics:
    * the given list contains the tuples:
      * the relative 'frames' duration (= pct *. length OR secs *. fps)
      * the y-value of the frame-index
        * .. interpolation is used to find the points between frame-indexes
    * the point '0., 0.' is implicitly always present
  * Advantages of this semantics:
    * you can easily express relative values from different types of data:
      * 'pct *. length'
      * 'secs *. fps'
    * you can't make an unordered list of absolute values by mistake
    * you can insert element in envelope without updating later values
*)
let points_array arr =
  let prev_abs_x = ref 0. in
  let arr_absx =
    arr |> CCArray.map (fun (rel_x, y) ->
      let abs_x = !prev_abs_x +. rel_x in
      prev_abs_x := abs_x;
      abs_x, y
    )
  in
  fun ~i ~v -> 
  (*> Note: depends on i being in between v2.x and v2'.x*)
  let interp_aux i v2 v2' =
    let diff_x = fst v2' -. fst v2 in
    let diff_y = snd v2' -. snd v2 in
    if CCFloat.equal diff_x 0. then
      snd v2
    else 
      let i_pct = (i -. fst v2) /. diff_x in
      let y_rel = i_pct *. diff_y in
      snd v2 +. y_rel
  in
  let i = float i in
  let cmp (x, _) (x', _) = CCFloat.compare x x' in
  let key = (i, 0.) in (*< Note, 0. is ignored according to 'cmp'*)
  match CCArray.bsearch ~cmp key arr_absx with
  | `At i_arr ->
    if CCArray.length arr_absx < i_arr+2 then
      0.
    else 
      interp_aux i arr_absx.(i_arr) arr_absx.(i_arr+1)
  | `All_lower -> 0.
  | `All_bigger ->
    if i >= 0. then
      interp_aux i (0., 0.) arr_absx.(0)
    else 0.
  | `Just_after i_arr ->
    interp_aux i arr_absx.(i_arr) arr_absx.(i_arr+1)
  | `Empty -> 0.

let points l = l |> CCArray.of_list |> points_array

let adsr a d s r = [ a; d; s; r ] |> points

let ramp ~length = points [ length, 1.0 ]
let ramp_rev ~length = points [ 0.0, 1.0; length, 0.0 ]

(*> Note: when using this with Inf.of_finite - pass a 'length' > square's 'length'
  .. to control distance between squares
*)
let square ~length = points [ 0., 1.0; length, 1.0 ]

let trace tag f ~i ~v =
  let r = f ~i ~v in
  Format.eprintf "envelope: i = %d, %s = %.2f\n%!" i tag r;
  r

type pure_anim = i:int -> v:unit -> float

module type FIELDS = sig

  type t
  val all : t list
  val compare : t -> t -> int

end

module Make_multi(Fields : FIELDS) = struct

  type dimensions = pure_anim array

  module type MAP = sig

    type field
    type t

    val init : (field -> pure_anim) -> t
    val get : field -> t -> pure_anim
    val map : (pure_anim -> pure_anim) -> t -> t

  end

  module Dimensions : MAP with type field = Fields.t = struct

    type t = dimensions
    type field = Fields.t

    let keys = CCArray.of_list Fields.all

    (*> Note: a map always has all keys*)
    let get k arr =
      let i, _ =
        CCArray.find_idx (fun k' -> Fields.compare k k' = 0) keys
        |> CCOption.get_exn_or "Anim_multi.get: key didn't exist (impossible)"
      in
      arr.(i)
      
    let map = CCArray.map
                
    let init init = keys |> CCArray.map init

  end

  type t = {
    dimensions : Dimensions.t;
    (*> goto should length become float like all other ~length params
      .. which also makes sense for user, and this code doesn't use the fact that it's
         an integer anyway
    *)
    length : int;
    duration : float;
  }

  let get k v = Dimensions.get k v.dimensions

  let map f v = { v with dimensions = v.dimensions |> Dimensions.map f }

  let init ~length ~duration init_dimensions =
    { length; duration; dimensions = Dimensions.init init_dimensions }

  module Compose = struct

    module Random_chronological_switch = struct

      (*Notes on the reasoning for this caching:
        * I don't have a pure and indexed version of Random that doesn't leak memory
          * a leaking version can be constructed - that also caches values
            * so would not be better, and one could more easily make the mistake of
              leaking
        * .. so this cache has been made instead
        * @why?
          * I want to compose a lot of animation-snippets based on different random
            seeds
          * each pig DOM object is initialized with the same random seed
          * => leading to _lots_ of Random initializations
          * .. and as this runs on my server, with potential lots of requests
            * I want this to be efficient
        * @note; with this global cache - the initializations are even cached
          across requests
          * @note; as CCCache is used, based on an LRU, then it's okay to call
            with random input-values
      *)

      module Cache_params = struct

        type t = {
          rand_seed : int;
          n_anims : int;
          resulting_length : int;
        } [@@deriving eq, show]

        let hash v = CCHash.(combine3
            (int v.rand_seed)
            (int v.n_anims)
            (int v.resulting_length)
        )

      end

      let make_anim_idxs =
        (*> Note: should be fine with 256 - just an arbitrary choice
          .. the most important thing is that calls are cached per request
             .. and there should not be more than 256 different sets of params
                used within this request *)
        (*> goto think about if this is okay to make global -
            possibly make into a state-generating wrapper*)
        let cache = CCCache.lru ~eq:Cache_params.equal ~hash:Cache_params.hash 256 in
        fun ~rand_seed ~n_anims ~resulting_length ->
          (*> goto remove dependence on deriving.std ppx?*)
          (* let cb ~in_cache i o = *)
          (*   if in_cache then *)
          (*     Format.eprintf "DEBUG: cache HIT!\n%!" *)
          (*   else  *)
          (*     Format.eprintf "DEBUG: cache updated! (%a)\n%!" Cache_params.pp *)
          (*       Cache_params.{ rand_seed; n_anims; resulting_length } *)
          (* in *)
          Cache_params.{ rand_seed; n_anims; resulting_length }
          |> CCCache.with_cache (* ~cb *) cache (fun params ->
            (*goto use CCRandom?*)
            let module R = Random.State in
            let r = R.make [| rand_seed |] in
            let rec aux ~prev_idx = function
              | 0 -> []
              | n -> 
                let idx =
                  (*> warning; I can't change values like this, as selected output
                      now depends on this - can make into param instead *)
                  (*> goto this _should_ anyway be a param for user to choose
                      .. @idea; could be a pure lambda taking the random float as input
                  *)
                  if R.float r 100. > 90. then
                    let idx = R.int r n_anims in
                    if idx = prev_idx then
                      succ idx mod n_anims
                    else idx
                  else
                    prev_idx
                in
                idx :: aux ~prev_idx:idx (pred n)
            in
            aux ~prev_idx:0 resulting_length
            (* |> CCList.rev  *)
            |> CCArray.of_list
          )

      let make ~rand_seed anims =
        let length =
          anims |> CCArray.fold_left (fun max_length v ->
            CCInt.max v.length max_length
          ) 0
        in
        let duration =
          anims |> CCArray.fold_left (fun max_duration v ->
            CCFloat.max v.duration max_duration
          ) 0.
        in
        let anim_idxs =
          make_anim_idxs
            ~rand_seed
            ~n_anims:(CCArray.length anims)
            ~resulting_length:length
        in
        init ~length ~duration (fun field ->
          fun ~i ~v ->
            let i = i mod length in
            let idx = anim_idxs.(i) in
            let anim = anims.(idx) in
            (get field anim) ~i ~v
        )

    end

    (*goto idea; try making a random-composition function that
      * choose random spots from each animation
      * change the rate of change based on some envelope arg?
    *)

  end

  (*> Note: ranges is a list of absolute time-positions that each specify a range*)
  let extract ~fps ~ranges anim =
    let ranges =
      ranges |> CCList.map (fun (start, stop) ->
        start *. fps, stop *. fps
      )
    in
    let anim_length = float anim.length in
    let dimensions =
      anim.dimensions |> Dimensions.map (fun anim ->
        (*> goto maybe make this into a flag-param - maybe one doesn't want looping*)
        let anim = Inf.of_finite ~length:anim_length anim in
        extract ~ranges anim
      )
    in
    let length =
      ranges |> CCList.fold_left (fun acc (start, stop) ->
        acc +. (stop -. start)
      ) 0.
    in
    let duration = length /. fps in
    let length = truncate length in
    { length; duration; dimensions }

  (* module Range_extraction *)

end

