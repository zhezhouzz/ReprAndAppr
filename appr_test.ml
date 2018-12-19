open Appr
open Repr
open Apprlist

let rec print_list_aux rl =
  let tmp = ReprlistInt.rmatch rl in
  match tmp with
  | None -> ""
  | Some (v, rl') -> string_of_int v ^ ", " ^ print_list_aux rl'

let print_list rl = Printf.printf "[%s]\n" (print_list_aux rl)

let rec sample_f l amatch =
  let label_f t =
    match t with None -> None | Some (v, _) -> if true then Some v else None
  in
  let tmp = amatch l label_f in
  match tmp with None -> () | Some (_, l') -> sample_f l' amatch

let rec sample_nonzero l amatch =
  let label_f t =
    match t with None -> None | Some (v, _) -> if v = 0 then None else Some v
  in
  let tmp = amatch l label_f in
  match tmp with None -> () | Some (_, l') -> sample_nonzero l' amatch

let rec diff1 rl arl =
  match ReprlistInt.rmatch rl with
  | None -> 0
  | Some (v, rl') -> (
    match ReprlistIntAppr.rmatch arl with
    | None -> v + diff1 rl' arl
    | Some (av, arl') -> abs (av - v) + diff1 rl' arl' )

let rec diff_nonzero rl arl =
  match ReprlistInt.rmatch rl with
  | None -> 0
  | Some (v, rl') -> (
    match ReprlistIntAppr.rmatch arl with
    | None -> if v = 0 then diff_nonzero rl' arl else v + diff_nonzero rl' arl
    | Some (av, arl') ->
        if v = 0 then diff_nonzero rl' arl'
        else abs (av - v) + diff_nonzero rl' arl' )

;;
let rl =
  ReprlistInt.create_of_list [1; 0; 0; 0; 0; 0; 0; 2; 3; 0; 0; 4; 6; 0; 0]
in
let _ = Printf.printf "Rlist:\n" in
let _ = print_list rl in
let samples = Appr.sample (sample_f rl) in
let _ = Printf.printf "Full Samples:\n" in
let _ = Appr.print_samples samples in
let arl = Appr.regression samples rl in
let _ = Printf.printf "Apprlist with Full Samples:\n" in
let _ = ReprlistIntAppr.print_list arl in
let samples_nonzero = Appr.sample (sample_nonzero rl) in
let _ = Printf.printf "Nonzero Samples:\n" in
let _ = Appr.print_samples samples_nonzero in
let arl_nonzero = Appr.regression samples_nonzero rl in
let _ = Printf.printf "Nonzero Samples:\n" in
let _ = ReprlistIntAppr.print arl_nonzero in
let _ = ReprlistIntAppr.print_list arl_nonzero in
let diff1_full = diff1 rl arl in
let diff1_nonzero = diff1 rl arl_nonzero in
let diff_n_full = diff_nonzero rl arl in
let diff_n_nonzero = diff_nonzero rl arl_nonzero in
let _ = Printf.printf "diff of full sample: %i\n" diff1_full in
let _ = Printf.printf "diff of nonzero sample: %i\n" diff1_nonzero in
let _ = Printf.printf "nonzero diff of full sample: %i\n" diff_n_full in
let _ = Printf.printf "nonzero diff of nonzero sample: %i\n" diff_n_nonzero in
let _ = Appr.random_init 10 in
let ls = Appr.random_lists 10 10 3 in
let rate =
  Appr.quickcheck ls sample_nonzero (fun rl arl ->
      let d = diff1 rl arl in
      d < 5 )
in
let _ =
  Printf.printf
    "QuickCheck: 10 random(0, 3) list with length 10, Nonezero sample, Test: sum of diff \
     between every element is less than 5, rate = %f\n"
    rate
in
let rate2 =
  Appr.quickcheck ls sample_f (fun rl arl ->
      let d = diff1 rl arl in
      d < 5 )
in
let _ =
  Printf.printf
    "QuickCheck: 10 random(0, 3) list with length 10, Full sample, Test: sum of diff \
     between every element is less than 5, rate = %f\n"
    rate2
in
()
