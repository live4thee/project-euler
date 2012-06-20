let solve_p6 num =
  let sum num_list = List.fold (fun acc n -> n + acc) 0 num_list in
  let nums = [1..num] in
  let sum_of_sqr = List.map (fun x -> x*x) nums |> sum in
  let sqr_of_sum = let s = sum nums in s*s
  in
    sqr_of_sum - sum_of_sqr

printfn "%d" (solve_p6 100)
