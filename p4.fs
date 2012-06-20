(* Count downwards will be better. *)
let solve_p4 min_num max_num =
  let nums = seq { for x in max_num .. -1 .. min_num do for y in max_num .. -1 .. x -> x*y } in
  let is_palindrom num =
    let rev_str (s:string) = new string(Array.rev (s.ToCharArray())) in
    let num_str_orig = sprintf "%d" num in
    let num_str_rev  = rev_str num_str_orig in
      num_str_orig = num_str_rev
  in
    Seq.max (Seq.filter is_palindrom nums)

printfn "%d" (solve_p4 100 999)
