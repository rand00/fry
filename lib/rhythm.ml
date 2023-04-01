
type t = bool list

module Euclidean = struct 

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

module Bool = struct
  
  let is_on ~tick rhythm =
    CCList.nth_opt rhythm (tick mod (List.length rhythm))
    |> CCOption.exists CCFun.id

  let mapi f l =
    l |> CCList.fold_left (fun (i, acc_rhythm) v ->
      if v then
        let acc_rhythm = Some (f i) :: acc_rhythm in
        succ i, acc_rhythm
      else
        i, acc_rhythm
    ) (0, [])
    |> snd
    |> CCList.rev

end

module Option = struct

  let get ~tick rhythm =
    CCList.nth_opt rhythm (tick mod (List.length rhythm))
    |> CCOption.flatten

  let is_on ~tick rhythm = get ~tick rhythm |> Option.is_some
  
  let mapi f l =
    l |> CCList.fold_left (fun (i, acc_rhythm) v ->
      match v with
      | None -> i, acc_rhythm
      | Some v -> 
        let acc_rhythm = Some (f i v) :: acc_rhythm in
        succ i, acc_rhythm
    ) (0, [])
    |> snd
    |> CCList.rev

end

let rotate_left ~n l =
  let n = n mod CCList.length l in
  let l, l' = CCList.take_drop n l in
  l' @ l

let rotate_right ~n l =
  CCList.rev l
  |> rotate_left ~n
  |> CCList.rev

let shuffle l =
  let a = CCArray.of_list l in
  CCArray.shuffle a;
  CCArray.to_list a

let drop_last n l =
  CCList.rev l
  |> CCList.drop n
  |> CCList.rev

let show_list l = CCList.to_string ~start:"[ " ~stop:" ]" ~sep:"; " l

let to_string r =
  let bool_to_string = function
    | true -> "true"
    | false -> "false"
  in
  show_list bool_to_string r

