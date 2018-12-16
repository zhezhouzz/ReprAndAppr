open Linear

;;
let linear = Linear.build 1 2 in
let _ = Linear.print linear in
let input = 3 in
let _ =
  Printf.printf "intput = %i, output = %i\n" input (Linear.apply linear input)
in
let pw = Piecewise.build (0, 0) [(0, 1, 1); (4, -3, 2)] 8 (0, 0) in
let _ = Piecewise.print pw in
let _ =
  Printf.printf "intput = %i, output = %i\n" 2 (Piecewise.apply pw 2) in
let _ =
  Printf.printf "intput = %i, output = %i\n" 6 (Piecewise.apply pw 6) in 
()
