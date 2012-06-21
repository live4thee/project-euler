let solve_p10 num =
  let sum_list ns = List.fold (fun acc (n: int) -> acc + int64(n)) 0L ns
  in Util.primes num |> sum_list

printfn "%d" (solve_p10 2000000)
