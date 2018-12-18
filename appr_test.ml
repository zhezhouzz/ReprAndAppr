open Appr
open Repr

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

let rec print_samples_aux l =
  match l with
  | [] -> ""
  | (idx, v) :: l' ->
      "(" ^ string_of_int idx ^ ", " ^ string_of_int v ^ "), "
      ^ print_samples_aux l'

let print_samples l = Printf.printf "[%s]\n" (print_samples_aux l)

;;
let rl = ReprlistInt.create_of_list [1; 2; 3; 4; 6] in
let _ = print_list rl in
let samples = Appr.sample (sample_f rl) in
let _ = print_samples samples in
()
