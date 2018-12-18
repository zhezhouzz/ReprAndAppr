open Linear

module ReprlistIntAppr = struct
  type sfunction = {s1: Linear.linear; s2: Range.range}

  type vfunction = Piecewise.piecewise

  type header = int

  type reprlist = {s: sfunction; v: vfunction; h: header}

  let build s1 s2 v h = {s= {s1; s2}; v; h}

  let composite g f idx = g (f idx)

  let rmatch (reprl : reprlist) : (int * reprlist) option =
    (* let _ = Printf.printf "header = %i\n" reprl.h in *)
    if Range.in_range reprl.s.s2 reprl.h then
      Some
        ( Piecewise.apply reprl.v reprl.h
        , {reprl with h= Linear.apply reprl.s.s1 reprl.h} )
    else None

  let rec rfold_left f start reprl =
    match rmatch reprl with
    | None -> start
    | Some (v, reprl') ->
        let start' = f start v in
        rfold_left f start' reprl'

  let rmap f reprl = {reprl with v= Piecewise.composite_linear f reprl.v}

  let length reprl = Range.length reprl.s.s2

  let reverse reprl =
    let s1' = Linear.reverse reprl.s.s1 in
    let len = length reprl in
    let h' = len - 1 in
    {reprl with h= h'; s= {reprl.s with s1= s1'}}

  let rec print_list_aux rl =
    let tmp = rmatch rl in
    match tmp with
    | None -> ""
    | Some (v, rl') -> string_of_int v ^ ", " ^ print_list_aux rl'

  let print_list rl = Printf.printf "[%s]\n" (print_list_aux rl)

  let print rl =
  Piecewise.print rl.v
end
