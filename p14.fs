let isOdd (num: bigint) = num % 2I = 1I
let nextNum (n: bigint) = if isOdd n then 3I * n + 1I else n / 2I

let numCache = new System.Collections.Generic.Dictionary<bigint, int>()

let rec get_series_counter (cur_num: bigint) =
  try
    numCache.[cur_num]
  with exn ->
    let next_num = nextNum cur_num in
    let count = get_series_counter next_num in
      numCache.Add(cur_num, count + 1);
      count + 1

let solve_p14 (max_num: bigint) =
  let rec solver cur_num acc_num acc_cnt =
    if cur_num > max_num then acc_num
    else
      let cnt = get_series_counter cur_num in
      let next_num = cur_num + 1I in
      let acc_num' = if cnt > acc_cnt then cur_num else acc_num in
      let acc_cnt' = if cnt > acc_cnt then cnt else acc_cnt in
        solver next_num acc_num' acc_cnt'
  in
    numCache.Add(1I, 1);
    numCache.Add(2I, 2);
    numCache.Add(3I, 8);
    numCache.Add(5I, 6);
    numCache.Add(7I, 17);
    solver 3I 2I 2

(* Works in F# interpretor.  Compiled EXE will stack overflow. *)
 printfn "%O" (solve_p14 1000000I)
