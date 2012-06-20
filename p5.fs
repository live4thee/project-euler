let solve_p5 num =
  let prod num_list = List.fold (fun acc n -> n * acc) 1 num_list in
  let prime_numbers = List.rev (Util.primes num) in
  let minimum = prod prime_numbers in
  let leftovers = List.filter (fun n -> minimum % n <> 0) [2..num] in
  let get_multiplier n =
    if n = 1 then 1
    else
      let tmp = List.find (fun x -> n % x = 0) prime_numbers
      in n / tmp
  in
  let reduce n num_list =
    List.map (fun x -> if x % n = 0 then x/n else x) num_list
  in
  let rec adjust acc new_leftovers =
    match new_leftovers with
        [] -> acc
      | n :: ns ->
          let multiplier = get_multiplier n in
            adjust (multiplier * acc) (reduce multiplier ns)
  in
    adjust minimum leftovers

printfn "%d" (solve_p5 20)
