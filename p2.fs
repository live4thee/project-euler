let solve_p2 max =
  let rec fib_gen p0 p1 acc =
    if p1 <= max then
      let p00 = p1 in
      let p11 = p0 + p1 in
        if p1 % 2 = 0 then fib_gen p00 p11 (p1 + acc)
        else fib_gen p00 p11 acc
    else acc
  in
    fib_gen 1 1 0

printfn "%d" (solve_p2 4000000)
