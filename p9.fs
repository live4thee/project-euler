let solve_p9 num =
  let check b =
    let numerator = num * (num - 2 * b) in
    let denominator = 2 * (num - b) in
      if numerator % denominator = 0 then (true, numerator / denominator)
      else (false, 0)
  in
  let max_b = if num % 2 = 0 then num / 2 else (num + 1) / 2 in
  let bs = [2 .. (max_b - 1)] in
  let candidates =
    List.fold (fun acc b ->
                 let (pred, a) = check b in
                   if pred && a < b then (a, b)::acc else acc) [] bs
  in
    List.iter (fun (a, b) ->
                 let c = num - a - b in
                   if a*a + b*b = c*c then printf "%d\n" (a*b*c) ) candidates

solve_p9 1000
