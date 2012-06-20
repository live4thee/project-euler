let solve_p10 num =
  let sum_list ns = List.fold (fun acc (n: int) -> acc + (new bigint(n))) 0I ns
  in Util.primes num |> sum_list

printfn "%O" (solve_p10 2000000)
