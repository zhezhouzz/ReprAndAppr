open Apprlist
open Linear

let rec print_list_aux rl =
  let tmp = ReprlistIntAppr.rmatch rl in
  match tmp with
  | None -> ""
  | Some (v, rl') -> string_of_int v ^ ", " ^ print_list_aux rl'

let print_list rl = Printf.printf "[%s]\n" (print_list_aux rl)

let plus_1 rl = ReprlistIntAppr.rmap (Linear.build 1 1) rl

let sum rl = ReprlistIntAppr.rfold_left (fun r elem -> r + elem) 0 rl

;;
let s1 = Linear.build 1 1 in
let s2 = (-1, 10) in
let v = Piecewise.build (0, 0) [(0, 1, 1); (4, -3, 2)] 10 (0, 0) in
let h = 0 in
let arl = ReprlistIntAppr.build s1 s2 v h in
let _ = print_list arl in
let arl_plus_1 = plus_1 arl in
let _ = print_list arl_plus_1 in
let _ = Printf.printf "%d\n" (sum arl) in
let arl_rev = ReprlistIntAppr.reverse arl in
let _ = print_list arl_rev in
()
