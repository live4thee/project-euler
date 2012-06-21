(* Find the largest prime factor of the number `num'. *)
let solve_p3 (num: int64) =
  let dividable (n: int) = num % int64(n) = 0L in
    Util.primes (num |> float |> sqrt |> int) |> List.find dividable

printfn "%d" (solve_p3 600851475143L)
