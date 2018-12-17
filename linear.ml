module Linear = struct
  type linear = {a0: int; a1: int}

  let build a0 a1 = {a0; a1}

  let print linear = Printf.printf "{a0 = %i; a1 = %i}\n" linear.a0 linear.a1

  let apply linear input = linear.a0 + (linear.a1 * input)

  let composite linear1 linear0 =
    {a0= linear1.a0 + (linear1.a1 * linear0.a0); a1= linear0.a1 * linear1.a1}

  let reverse linear = {a0= -1 * linear.a0 / linear.a1; a1= 1 / linear.a1}
end

module Range = struct
  type range = int * int

  let in_range range idx =
    match range with l, r -> if l < idx && idx <= r then true else false

  let length range = match range with l, r -> r - l
end

module Piecewise = struct
  include Linear
  include Range

  type piece = {range: range; linear: linear}

  type halfpiece = {bound: int; linear: linear}

  type piecewise = {piecelist: piece list; left: halfpiece; right: halfpiece}

  let build_piece left right a0 a1 =
    {range= (left, right); linear= Linear.build a0 a1}

  let rec build_piecelist plist right_bound =
    match plist with
    | [] -> []
    | (left, a0, a1) :: t -> (
      match t with
      | [] -> [build_piece left right_bound a0 a1]
      | (left', _, _) :: _ ->
          let piece = build_piece left left' a0 a1 in
          piece :: build_piecelist t right_bound )

  let build ldefault plist right_bound rdefault =
    match ldefault with la0, la1 -> (
      match rdefault with ra0, ra1 -> (
        let left_default = Linear.build la0 la1 in
        let right_default = Linear.build ra0 ra1 in
        match plist with
        | [] ->
            let left = {bound= right_bound; linear= left_default} in
            let right = {bound= right_bound; linear= right_default} in
            {piecelist= []; left; right}
        | (left_bound, _, _) :: _ ->
            let left = {bound= left_bound; linear= left_default} in
            let right = {bound= right_bound; linear= right_default} in
            let p = build_piecelist plist right_bound in
            {piecelist= p; left; right} ) )

  let print_piece p =
    match p.range with left, right ->
      Printf.printf "In (%i, %i], y = %i + %i*x;\n" left right p.linear.a0
        p.linear.a1

  let rec print_plist plist =
    match plist with
    | [] -> ()
    | piece :: t ->
        let _ = print_piece piece in
        print_plist t

  let print pw =
    let _ =
      Printf.printf "In (-inf, %i], y = %i + %i*x;\n" pw.left.bound
        pw.left.linear.a0 pw.left.linear.a1
    in
    let _ = print_plist pw.piecelist in
    let _ =
      Printf.printf "In (%i, inf), y = %i + %i*x;\n" pw.right.bound
        pw.right.linear.a0 pw.right.linear.a1
    in
    ()

  exception BADERROR

  let rec apply_aux plist input =
    match plist with
    | [] -> raise BADERROR
    | piece :: t ->
        if in_range piece.range input then Linear.apply piece.linear input
        else apply_aux t input

  let apply pw input =
    if input <= pw.left.bound then Linear.apply pw.left.linear input
    else if input > pw.right.bound then Linear.apply pw.right.linear input
    else apply_aux pw.piecelist input

  let composite_linear f pw =
    let left = {pw.left with linear= Linear.composite f pw.left.linear} in
    let right = {pw.right with linear= Linear.composite f pw.right.linear} in
    let plist' =
      List.map
        (fun (piece : piece) ->
          {piece with linear= Linear.composite f piece.linear} )
        pw.piecelist
    in
    {piecelist= plist'; left; right}
end
