let solve_p7 num =
  let prime_array = Array.zeroCreate num in
  let next_guess guess = if guess % 2 = 0 then guess + 1 else guess + 2 in
  let rec next_prime guess =
    if Array.exists (fun prime_num ->
                       if prime_num = 0 then false
                       else guess % prime_num = 0) prime_array
    then next_prime (next_guess guess)
    else guess
  in
  let rec find_nth_prime cnt =
    let idx = cnt - 1 in
    if cnt = num then prime_array.[idx]
    else
      (prime_array.[cnt] <- next_prime (next_guess prime_array.[idx]);
       find_nth_prime (cnt + 1))
  in
    prime_array.[0] <- 2;
    find_nth_prime 1

printfn "%d" (solve_p7 10001)
