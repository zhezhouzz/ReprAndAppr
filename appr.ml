open Linear
open Apprlist
open Repr
open Linear

module Appr = struct
  type samples = (int * int) list

  let sample
      (f :
           (   ReprlistInt.reprlist
            -> ((int * ReprlistInt.reprlist) option -> int option)
            -> (int * ReprlistInt.reprlist) option)
        -> unit) =
    let reflist = ref [] in
    let update_rmatch l if_use_f =
      let idx, tmp = ReprlistInt.rmatch2 l in
      let scalar = if_use_f tmp in
      match scalar with
      | None -> tmp
      | Some v ->
          (* let _ = Printf.printf "v = %i\n" v in  *)
          let _ = reflist := !reflist @ [(idx, v)] in
          tmp
    in
    let _ = f update_rmatch in
    !reflist

  let rec print_samples_aux l =
    match l with
    | [] -> ""
    | (idx, v) :: l' ->
        "(" ^ string_of_int idx ^ ", " ^ string_of_int v ^ "), "
        ^ print_samples_aux l'

  let print_samples l = Printf.printf "[%s]\n" (print_samples_aux l)

  exception BAD_ERROR

  let rec regression_aux (samples : samples) =
    match samples with
    | [] -> raise BAD_ERROR
    | (idx0, v0) :: samples' -> (
      match samples' with
      | [] -> ([], idx0, (float_of_int v0, 0.0))
      | (idx1, v1) :: samples'' ->
          let a0, a1 = Linear.line (idx0, v0) (idx1, v1) in
          let l, rbound, rlinear = regression_aux samples' in
          ((idx0, a0, a1) :: l, rbound, rlinear) )

  let regression (samples : samples) rl =
    let len = ReprlistInt.length rl in
    let s1 = Linear.build 1.0 1.0 in
    let h = 0 in
    match samples with
    | [] ->
        let s2 = (0, 0) in
        let v = Piecewise.build (0.0, 0.0) [] 0 (0.0, 0.0) in
        ReprlistIntAppr.build s1 s2 v h
    | (idx0, v0) :: samples' ->
        let l, rbound, rlinear = regression_aux samples in
        let v = Piecewise.build (float_of_int v0, 0.0) l rbound rlinear in
        let s2 = (-1, len - 1) in
        ReprlistIntAppr.build s1 s2 v h

  let check (l : int list) sample_f
      (test_f : ReprlistInt.reprlist -> ReprlistIntAppr.reprlist -> bool) =
    let rl = ReprlistInt.create_of_list l in
    let samples = sample (sample_f rl) in
    let arl = regression samples rl in
    test_f rl arl

  let rec quickcheck_aux (ls : int list list) f =
    match ls with
    | [] -> (0, 0)
    | l :: ls' ->
        let len, passed = quickcheck_aux ls' f in
        if f l then (len + 1, passed + 1) else (len + 1, passed)

  let quickcheck (ls : int list list)
      (sample_f :
           ReprlistInt.reprlist
        -> (   ReprlistInt.reprlist
            -> ((int * ReprlistInt.reprlist) option -> int option)
            -> (int * ReprlistInt.reprlist) option)
        -> unit)
      (test_f : ReprlistInt.reprlist -> ReprlistIntAppr.reprlist -> bool) =
    let f l = check l sample_f test_f in
    let len, passed = quickcheck_aux ls f in
    float_of_int passed /. float_of_int len

  let random_init i = Random.init i

  let rec random_list len bound =
    if len = 0 then [] else Random.int bound :: random_list (len - 1) bound

  let rec random_lists len num bound =
    if num = 0 then []
    else random_list len bound :: random_lists len (num - 1) bound
end
