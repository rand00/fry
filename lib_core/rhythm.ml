
module T = struct 

  type 'a t = 'a list

  type 'a context = {
    note : 'a;
    tick : int; (*< Note: duplicate information, but useful*)
    rhythm_index : int;
    note_index : int;
  }
  
end
include T

let with_context ~tick ~len_rhythm note = {
  note;
  tick;
  rhythm_index = tick / len_rhythm;
  note_index = tick mod len_rhythm;
}

let rotate_left n l =
  let n = n mod CCList.length l in
  let l, l' = CCList.take_drop n l in
  l' @ l

let rotate_right n l =
  CCList.rev l
  |> rotate_left n
  |> CCList.rev

let shuffle l =
  let a = CCArray.of_list l in
  CCArray.shuffle a;
  CCArray.to_list a

let drop_last n l =
  CCList.rev l
  |> CCList.drop n
  |> CCList.rev

let index ~tick rhythm = tick mod List.length rhythm

let to_string elem_to_string =
  CCList.to_string ~start:"[ " ~stop:" ]" ~sep:"; " elem_to_string

module Bool = struct

  type t = bool T.t
  
  let is_on ~tick rhythm =
    CCList.nth_opt rhythm (tick mod (List.length rhythm))
    |> CCOption.exists CCFun.id

  let count rhythm = CCList.count CCFun.id rhythm
  
  let mapi f l =
    l |> CCList.fold_left (fun (i, acc_rhythm) v ->
      if v then
        let acc_rhythm = Some (f i) :: acc_rhythm in
        succ i, acc_rhythm
      else
        let acc_rhythm = None :: acc_rhythm in
        i, acc_rhythm
    ) (0, [])
    |> snd
    |> CCList.rev

  let to_string v =
    let bool_to_string = function
      | true -> "true"
      | false -> "false"
    in
    to_string bool_to_string v

  let o = false
  let x = true

end

module Option = struct

  type 'a t = 'a option T.t
  
  let get_aux ~tick ~len_rhythm rhythm =
    CCList.nth_opt rhythm (tick mod len_rhythm)
    |> CCOption.flatten

  let get ~tick rhythm =
    let len_rhythm = List.length rhythm in
    get_aux ~tick ~len_rhythm rhythm

  let get_with_context ~tick rhythm =
    let len_rhythm = List.length rhythm in
    get_aux ~tick ~len_rhythm rhythm |> CCOption.map (fun note ->
      with_context ~tick ~len_rhythm note
    )
  
  let is_on ~tick rhythm = get ~tick rhythm |> Option.is_some
  
  let count rhythm = CCList.count CCOption.is_some rhythm
  
  let mapi f l =
    l |> CCList.fold_left (fun (i, acc_rhythm) v ->
      match v with
      | None ->
        let acc_rhythm = None :: acc_rhythm in
        i, acc_rhythm
      | Some v -> 
        let acc_rhythm = Some (f i v) :: acc_rhythm in
        succ i, acc_rhythm
    ) (0, [])
    |> snd
    |> CCList.rev

  let to_string elem_to_string v =
    let aux = function
      | None -> "None"
      | Some v -> elem_to_string v
    in
    to_string aux v

end

module Euclidean = struct 

  type t = bool T.t

  let split_on_change ~changed l =
    let rec aux acc = function
      | [] -> CCList.rev acc, None
      | hd :: tl ->
        match CCList.head_opt acc with
        | Some hd' when changed hd hd' -> acc, Some (hd :: tl)
        | _ -> aux (hd :: acc) tl
    in
    aux [] l

  let rec join_pairwise l l' = match l, l' with
    | hd :: tl, hd' :: tl' -> (hd @ hd') :: join_pairwise tl tl'
    | [], rest | rest, []  -> rest

  (*Bjorklunds algorithm http://cgm.cs.mcgill.ca/~godfried/publications/banff.pdf*)
  let make ~len ~n =
    (*> Note: I do first part this way to keep rest of algorithm concise*)
    let init = List.init (CCInt.max n (len-n)) (fun i ->
      let beat = i < n in
      let beat_l = if beat then [ beat ] else [] in
      let empty = i < (len-n) in
      let empty_l = if empty then [ false ] else [] in
      beat_l @ empty_l
    )
    in
    let rec aux l =
      let longs, shorts_opt =
        split_on_change l ~changed:(fun x y -> CCList.compare_lengths x y <> 0)
      in
      match shorts_opt with
      | None           -> CCList.concat longs
      | Some [ short ] -> CCList.concat longs @ short
      (*< ... Bjorklund stops algorithm here... :/*)
      | Some shorts    -> aux (join_pairwise longs shorts)
    in
    aux init

(* let test_euclidian =
 *   assert (euclidean ~len:5 ~n:3 = [true; false; true; false; true]);
 *   assert (euclidean ~len:5 ~n:4 = [true; false; true; true; true]);
 *   assert (euclidean ~len:8 ~n:6 = [true; false; true; true; true; false; true; true ]) *)

end

module Poly = struct

  (*> Note: A helper to make a set of polyrhythms into a single static one.
      .. the resulting rhythm will have length = GCD lengths
      * warning: all rhythms should have length > 0
  *)
  let merge ~f rhythms =
    let rhythms_arr =
      rhythms
      |> CCArray.of_list
      |> CCArray.map CCArray.of_list
    in
    assert (rhythms_arr |> CCArray.for_all (fun a -> CCArray.length a > 0));
    let rec aux i acc =
      let note_idxs =
        rhythms_arr
        |> CCArray.map (fun rhythm -> i mod CCArray.length rhythm)
      in
      let notes =
        rhythms_arr
        |> CCArray.mapi (fun i_rhythm rhythm -> rhythm.(note_idxs.(i_rhythm)))
        |> CCArray.to_list
      in
      let v = f notes in
      if i <> 0 && CCArray.for_all (CCInt.equal 0) note_idxs then
        acc
      else 
        let acc = v :: acc in
        aux (succ i) acc
    in
    aux 0 []
    |> CCList.rev

end
