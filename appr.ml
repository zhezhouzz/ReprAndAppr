open Linear
open Apprlist
open Repr

module Appr = struct
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
          let _ = Printf.printf "v = %i\n" v in 
          let _ = reflist := (idx, v) :: !reflist in
          tmp
    in
    let _ = f update_rmatch in
    !reflist
end
