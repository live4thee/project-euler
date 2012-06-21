let isOdd (num: int64) = num % 2L = 1L
let nextNum (n: int64) = if isOdd n then 3L * n + 1L else n / 2L

let numCache = new System.Collections.Generic.Dictionary<int64, int>()

let rec get_series_counter (cur_num: int64) =
  if numCache.ContainsKey(cur_num) then numCache.[cur_num]
  else
    let next_num = nextNum cur_num in
    let count = get_series_counter next_num in
      numCache.Add(cur_num, count + 1);
      count + 1

let solve_p14 (max_num: int64) =
  let rec solver cur_num acc_num acc_cnt =
    if cur_num > max_num then acc_num
    else
      let cnt = get_series_counter cur_num in
      let next_num = cur_num + 1L in
      let acc_num' = if cnt > acc_cnt then cur_num else acc_num in
      let acc_cnt' = if cnt > acc_cnt then cnt else acc_cnt in
        solver next_num acc_num' acc_cnt'
  in
    numCache.Add(1L, 1);
    numCache.Add(2L, 2);
    numCache.Add(3L, 8);
    numCache.Add(5L, 6);
    numCache.Add(7L, 17);
    solver 3L 2L 2

printfn "%d" (solve_p14 1000000L)
