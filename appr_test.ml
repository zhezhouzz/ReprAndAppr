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


;;
let rl = ReprlistInt.create_of_list [1; 0; 0; 0; 0; 0; 0; 2; 3; 0; 0; 4; 6; 0; 0] in
let _ = print_list rl in
let samples = Appr.sample (sample_f rl) in
let _ = Appr.print_samples samples in
let arl = Appr.regression samples rl in
let _ = ReprlistIntAppr.print_list arl in
let samples_nonzero = Appr.sample (sample_nonzero rl) in
let _ = Appr.print_samples samples_nonzero in
let arl_nonzero = Appr.regression samples_nonzero rl in
let _ = ReprlistIntAppr.print arl_nonzero in
let _ = ReprlistIntAppr.print_list arl_nonzero in
()
