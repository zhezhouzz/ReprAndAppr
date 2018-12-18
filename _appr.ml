module type Reprlist = sig

  type oneargfun

  type twoargfun

  type sfunction

  type vfunction

  type header

  type reprlist = sfunction * vfunction * header

  val create_of_list : int list -> reprlist

  val rmatch : reprlist -> (int * reprlist) option

  val rfold_left : twoargfun -> int -> reprlist -> int

  val rmap : oneargfun -> reprlist -> reprlist

  val length : reprlist -> int

  val reverse : reprlist -> reprlist

  val nth : reprlist -> int -> int option
end

module ReprlistInt : Reprlist = struct
  type sfunction = (int -> int) * (int -> int option)

  type vfunction = int -> int

  type header = int

  type reprlist = sfunction * vfunction * header

  type twoargfun = int -> int -> int

  type oneargfun = int -> int

  let composite g f idx = g (f idx)

  let make_sfunction sfun1 sfun2: sfunction = (sfun1, sfun2)

  let rec vfunction_gen l i =
     match l with
     | [] -> (fun idx -> 0)
     | h :: t -> (fun idx -> if idx == i then h else (vfunction_gen t (i + 1)) idx)

  let create_of_list (l: int list) =
     let sfun1 idx = idx + 1 in
     let len = List.length l in
     let sfun2 idx = if idx >= len then None else Some idx in
     let vfun = vfunction_gen l 0 in
     ((sfun1, sfun2), vfun, 0)
     

  let rmatch (reprl : reprlist) : (int * reprlist) option =
    match reprl with (sfun1, sfun2), vfun, idx -> (
      match sfun2 idx with
      | None -> None
      | Some idx'' -> Some (vfun idx, ((sfun1, sfun2), vfun, (sfun1 idx))) )

  let rec rfold_left f start reprl =
    match rmatch reprl with
    | None -> start
    | Some (v, reprl') ->
        let start' = f start v in
        rfold_left f start' reprl'

  let rmap f reprl =
    match reprl with sfun, vfun, idx -> (sfun, composite f vfun, idx)

  let rec length_rec sfun idx len =
    match sfun with sfun1, sfun2 -> (
      let idx' = sfun1 idx in
      match sfun2 idx' with
      | None -> len
      | Some idx'' -> length_rec sfun idx'' (len + 1) )

  let length reprl =
    match reprl with sfun, vfun, idx -> length_rec sfun idx 0

  let reverse reprl =
    let len = length reprl in
    match reprl with (sfun1, sfun2), vfun, idx ->
      ( ( (fun i -> len - sfun1 (len - i))
        , fun i -> if i <= 0 then None else Some i )
      , vfun
      , len )

  let rec nth reprl n =
    match rmatch reprl with
    | None -> None
    | Some (v, reprl') -> if n = 0 then Some v else nth reprl' (n - 1)
end

module Reprlist_int_appr = struct
type range = int * int
type linearfun = int * int
type sfun1 = (range* linearfun) list * linearfun
type sfunction = sfun1 *  (int -> int option)
type vfunction = (range* linearfun) list * linearfun
type header = int
type reprlist = sfunction * vfunction * header
type oneargfun = linearfun
type twoargfun = (int -> int -> int) * linearfun

let in_range range idx =
match range with
| l, r ->  if (l < idx) && (idx <= r) then true else false

(* type location =
 * | locleft
 * | locright
 * | locoverlap
 * | locsame *)

(* let check_location range1 range2 =
 * match range1 with l1, r1 ->
 * match range2 with l2, r2 ->
 * if (l1 = l2) && (r1 = r2) then locsame
 * else if r1 <= l2 then locleft
 * else if l2 >= r1 then locright
 * else locoverlap *)

let apply_linearfun linearfun x =
match linearfun with
| (a0, a1) -> a0 + a1* x

let rec apply_funlist funlist idx =
match funlist with
| l, default -> 
match l with
| [] -> apply_linearfun default idx
| (range, linearfun) :: l'->
if in_range range idx then apply_linearfun linearfun idx
else apply_funlist (l', default) idx

  let rmatch (reprl : reprlist) : (int * reprlist) option =
    match reprl with (sfun1, sfun2), vfun, idx -> (
      match sfun2 idx with
      | None -> None
      | Some idx'' -> Some (apply_funlist vfun idx, ((sfun1, sfun2), vfun, (apply_funlist sfun1 idx))) )

  let composite_linear linear2 linear1 =
  match linear1 with a00, a01 ->
match linear2 with a10, a11 ->
((a10 + a11*a00), a01*a11)

exception BAD_RANGE

(* let calibration_range range1 range2 =
 * match range1 with l1, r1 ->
 * match range2 with l2, r2 ->
 * if l1 = l2 then
 * (if r1 < r2 then ([range1], [(l2, r1); (r1, r2)])
 * else if r1 > r2 then ([(l1, r2);(r2, r1)],[range2])
 * else raise BAD_RANGE)
 * else if l1 < l2 then
 * (if r1 = r2 then ([(l1, l2);(l2, r1)], [range2])
 * else if r1 < r2 then ([(l1, l2);(l2, r1)],[(l2, r1);(r1, r2)])
 * else if r1 > r2 then ([(l1,l2);(l2,r2);(r2,r1)],[range2]))
 * else if l1 > l2 then
 * let range1', range2' = calibration_range range2 range1 in
 * (range2', range1')
 * 
 * let rec fill_range_with_linear ranges linear =
 * match ranges with
 * | [] -> []
 * | h :: t -> (h, linear) :: (fill_range_with_linear t linear)
 * 
 * let rec calibrarion_linearfun linearfun2 linearfun1 = 
 * match linearfun1 with linearlist1, default1 ->
 * match linearfun2 with linearlist2, default2 ->
 * match linearlist1 with
 * | [] -> (match linearlist2 with
 * | [] -> ([], composite_linear default2 default1)
 * | (range2, linear2)::t2 ->
 * let new_segment = (range2, (composite_linear linear2 default1)) in
 * let l', default' = calibrarion_linearfun (t2, default2) linearfun1 in
 * (new_segment::l', default'))
 * | (range1, linear1)::t1 ->
 * match linearlist2 with
 * | [] -> let new_segment = (range1, (composite_linear default2 linear1)) in
 * let l', default' = calibrarion_linearfun linearfun2 (t1, default1) in
 * (new_segment::l', default')
 * | (range2, linear2)::t2 ->
 * let loc = check_location range1 range2 in
 * match loc with
 * | locleft -> calibrarion_linearfun linearfun2 (t1, default1)
 * | locright -> calibrarion_linearfun (t2, default2) linearfun1
 * | locsame ->
 * let new_segment = (range1, (composite_linear linear2 linear1)) in
 * let l', default' = calibrarion_linearfun (t2, default2) (t1, default1) in
 * (new_segment::l', default')
 * | locoverlap ->
 * let range1', range2' = calibration_range range1 range2 in
 * let list1 = fill_range_with_linear range1 linear1 in
 * let list2 = fill_range_with_linear range2 linear2 in
 * calibrarion_linearfun (list2@t2, default2) (list1@t1, default1) *)

let rec composite_vfun_linear linear vfun =
  match vfun with vlist, default ->
  match vlist with
  | [] -> ([], composite_linear linear default)
  | (range, h) :: t ->
let vlist', default' = composite_vfun_linear linear (t, default) in
((range, (composite_linear linear h))::vlist', default')


  let rmap linear (reprl:reprlist): reprlist =  
    match reprl with sfun, vfun, idx ->
    (sfun, (composite_vfun_linear linear vfun), idx)

  let rec rfold_left_aux f start reprl =
    match rmatch reprl with
    | None -> start
    | Some (v, reprl') ->
        let start' = f start v in
        rfold_left_aux f start' reprl' 

  let rec rfold_left f start reprl =
    match f with linear_r, linear_elem ->
let reprl' = rmap linear_elem reprl in
rfold_left_aux linear_r start reprl'

  let rec length_rec sfun idx len =
    match sfun with sfun1, sfun2 -> (
      let idx' = sfun1 idx in
      match sfun2 idx' with
      | None -> len
      | Some idx'' -> length_rec sfun idx'' (len + 1) )

  let length reprl =
    match reprl with sfun, vfun, idx -> length_rec sfun idx 0

  let reverse_sfun1 sfun1 =
  ([], (1, -1))

  let reverse reprl =
    let len = length reprl in
    match reprl with (sfun1, sfun2), vfun, idx ->
      ( ( reverse_sfun1 sfun1
        , fun i -> if i <= 0 then None else Some i )
      , vfun
      , len )

  let rec nth reprl n =
    match rmatch reprl with
    | None -> None
    | Some (v, reprl') -> if n = 0 then Some v else nth reprl' (n - 1)

end
