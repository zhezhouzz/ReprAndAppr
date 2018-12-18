module type Reprlist = sig
  type sfunction

  type vfunction

  type header

  type reprlist = {s: sfunction; v: vfunction; h: header}

  val create_of_list : int list -> reprlist

  val rmatch : reprlist -> (int * reprlist) option

  val rmatch2 : reprlist -> int * (int * reprlist) option

  val rfold_left : (int -> int -> int) -> int -> reprlist -> int

  val rmap : (int -> int) -> reprlist -> reprlist

  val length : reprlist -> int

  val reverse : reprlist -> reprlist

  val nth : reprlist -> int -> int option
end

module ReprlistInt = struct
  type sfunction = {s1: int -> int; s2: int -> bool}

  type vfunction = int -> int

  type header = int

  type reprlist = {s: sfunction; v: vfunction; h: header}

  let composite g f idx = g (f idx)

  let rec vfunction_gen l i =
    match l with
    | [] -> fun idx -> 0
    | h :: t ->
        fun idx -> if idx == i then h else (vfunction_gen t (i + 1)) idx

  let create_of_list (l : int list) =
    let sfun1 idx = idx + 1 in
    let len = List.length l in
    let sfun2 idx = idx < len in
    let vfun = vfunction_gen l 0 in
    {s= {s1= sfun1; s2= sfun2}; v= vfun; h= 0}

  let rmatch (reprl : reprlist) : (int * reprlist) option =
    if reprl.s.s2 reprl.h then
      Some (reprl.v reprl.h, {reprl with h= reprl.s.s1 reprl.h})
    else None

  let rmatch2 (reprl : reprlist) : int * (int * reprlist) option =
    if reprl.s.s2 reprl.h then
      (reprl.h, Some (reprl.v reprl.h, {reprl with h= reprl.s.s1 reprl.h}))
    else (reprl.h, None)

  let rec rfold_left f start reprl =
    match rmatch reprl with
    | None -> start
    | Some (v, reprl') ->
        let start' = f start v in
        rfold_left f start' reprl'

  let rmap f reprl = {reprl with v= composite f reprl.v}

  let rec length_rec sfun idx len =
    if sfun.s2 idx then length_rec sfun (sfun.s1 idx) (len + 1) else len

  let length reprl = length_rec reprl.s reprl.h 0

  let reverse reprl =
    let len = length reprl in
    { reprl with
      s=
        { s1= (fun i -> len - 1 - reprl.s.s1 (len - 1 - i))
        ; s2= (fun i -> reprl.s.s2 (len - 1 - i)) }
    ; h= len - 1 }

  let rec nth reprl n =
    match rmatch reprl with
    | None -> None
    | Some (v, reprl') -> if n = 0 then Some v else nth reprl' (n - 1)
end
