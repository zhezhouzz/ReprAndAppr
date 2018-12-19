let rec sample_gt_0 (l : int list) =
  match l with
  | [] -> ()
  | v :: l' ->
      if v > 0 then
        let _ = Printf.printf "Sample %i\n" v in
        sample_gt_0 l'
      else sample_gt_0 l'

let place io =
  match io with
  | None -> ()
  | Some v ->
      let _ = Printf.printf "Sample %i\n" v in
      ()

let rec sample_gt_0_p (l : int list) (place : int option -> unit) =
  match l with
  | [] -> ()
  | v :: l' ->
      let _ = if v > 0 then place (Some v) else place None in
      sample_gt_0_p l' place

let repr_match (l : int list) : (int * int list) option =
  match l with [] -> None | v :: l' -> Some (v, l')

let rec sample_gt_0_r (l : int list)
    (rmatch : int list -> (int * int list) option) (place : int option -> unit)
    =
  match rmatch l with
  | None -> ()
  | Some (v, l') ->
      let _ = if v > 0 then place (Some v) else place None in
      sample_gt_0_r l' rmatch place

let repr_match' (l : int list)
    (decision : (int * int list) option -> int option) :
    (int * int list) option =
  let tmp = match l with [] -> None | v :: l' -> Some (v, l') in
  let d = decision tmp in
  let _ = place d in
  tmp

let rec sample_gt_0_r' (l : int list)
    (rmatch :
         int list
      -> ((int * int list) option -> int option)
      -> (int * int list) option) =
  (* decision part *)
  let decision tmp =
    match tmp with
    | None -> None
    | Some (v, _) -> if v > 0 then Some v else None
  in
  (* traverse part *)
  match rmatch l decision with
  | None -> ()
  | Some (_, l') -> sample_gt_0_r' l' rmatch

;;
let _ = sample_gt_0 [1; 0; 1; 2; 3; 0] in
let _ = sample_gt_0_p [1; 0; 1; 2; 3; 0] place in
let _ = sample_gt_0_r [1; 0; 1; 2; 3; 0] repr_match place in
let _ = sample_gt_0_r' [1; 0; 1; 2; 3; 0] repr_match' in
()
