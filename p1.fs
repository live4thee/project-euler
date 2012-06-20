let solve_p1 min max =
  let mod_of_3_or_5 n = (n % 3 = 0) || (n % 5 = 0) in
  let rec helper num acc =
    if num >= max then acc
    else
      if mod_of_3_or_5 num then helper (num + 1) (num + acc)
      else helper (num + 1) acc
  in
    helper min 0

printfn "%d" (solve_p1 1 1000)
