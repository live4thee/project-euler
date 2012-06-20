(* Find the largest prime factor of the number `num'. *)
let solve_p3 (num: bigint) =
  let dividable (n: int) = num % (new bigint(n)) = 0I in
    Util.primes (num |> float |> sqrt |> int) |> List.find dividable

printfn "%d" (solve_p3 600851475143I)
