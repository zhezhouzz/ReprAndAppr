open Repr;;

let rec print_list_aux rl =
let tmp = ReprlistInt.rmatch rl in
match tmp with
| None -> ""
| Some (v, rl') -> string_of_int v ^ ", " ^ print_list_aux rl'

let print_list rl =
Printf.printf "[%s]\n" (print_list_aux rl)

let plus_1 rl =
ReprlistInt.rmap (fun i -> i + 1) rl

let sum rl =
ReprlistInt.rfold_left (fun r elem -> r + elem) 0 rl

;;
let rl = ReprlistInt.create_of_list [1;2;3;4;5] in
let _ = print_list rl in
let rl_plus_1 = plus_1 rl in
let _ = print_list rl_plus_1 in
let _ = Printf.printf "%d\n" (sum rl) in
let rl_rev = ReprlistInt.reverse rl in
let _ = print_list rl_rev in
();;
