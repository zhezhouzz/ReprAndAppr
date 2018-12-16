module type Reprlist = sig

  type sfunction

  type vfunction

  type header

  type reprlist = sfunction * vfunction * header

  val create_of_list : int list -> reprlist

  val rmatch : reprlist -> (int * reprlist) option

  val rfold_left : (int -> int -> int) -> int -> reprlist -> int

  val rmap : (int -> int) -> reprlist -> reprlist

  val length : reprlist -> int

  val reverse : reprlist -> reprlist

  val nth : reprlist -> int -> int option
end

module ReprlistInt = struct
  type sfunction = (int -> int) * (int -> bool)

  type vfunction = int -> int

  type header = int

  type reprlist = sfunction * vfunction * header

  let composite g f idx = g (f idx)

  let make_sfunction sfun1 sfun2: sfunction = (sfun1, sfun2)

  let rec vfunction_gen l i =
     match l with
     | [] -> (fun idx -> 0)
     | h :: t -> (fun idx -> if idx == i then h else (vfunction_gen t (i + 1)) idx)

  let create_of_list (l: int list) =
     let sfun1 idx = idx + 1 in
     let len = List.length l in
     let sfun2 idx = idx < len in
     let vfun = vfunction_gen l 0 in
     ((sfun1, sfun2), vfun, 0)
     

  let rmatch (reprl : reprlist) : (int * reprlist) option =
    match reprl with (sfun1, sfun2), vfun, idx -> (
      if sfun2 idx then Some (vfun idx, ((sfun1, sfun2), vfun, (sfun1 idx))) else None )

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
      if sfun2 idx then length_rec sfun (sfun1 idx) (len + 1) else len )

  let length reprl =
    match reprl with sfun, vfun, idx -> length_rec sfun idx 0

  let reverse reprl =
    let len = length reprl in
    match reprl with (sfun1, sfun2), vfun, idx ->
      ( ( (fun i -> (len - 1) - sfun1 (len - 1 - i))
        , fun i -> sfun2 (len - 1 - i) )
      , vfun
      , len - 1 )

  let rec nth reprl n =
    match rmatch reprl with
    | None -> None
    | Some (v, reprl') -> if n = 0 then Some v else nth reprl' (n - 1)
end

