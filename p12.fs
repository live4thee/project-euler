let count_divisor (number: int) =
  let new_cnt_gen cur_num cnt =
    if number % cur_num <> 0 then (false, cnt)
    else (true, cnt + 2)
  in
  let adjust_count n cnt =
    if n * n <> number then cnt
    else cnt - 1
  in
  let rec counter last_ok_num cur_num num_limit cnt next_num_gen =
    if cur_num > num_limit then adjust_count last_ok_num cnt
    else
      let next_num = (next_num_gen cur_num) in
      let (ok, new_cnt) = new_cnt_gen cur_num cnt in
        if ok then
          counter cur_num next_num num_limit new_cnt next_num_gen
        else
          counter last_ok_num next_num num_limit new_cnt next_num_gen
  in
    if number = 1 then 1
    else
      let n_limit = number |> float |> sqrt |> floor |> int in
      let num_gen =
        if number % 2 = 0 then (fun n -> n + 1) else (fun n -> n + 2)
      in counter 1 (num_gen 1) n_limit 2 num_gen

let solve_p12 (cnt: int) =
  let triangle n = n * (n + 1) / 2 in
  let rec find_first cur_num addend =
    let next_num = cur_num + addend in
    let next_addend = addend + 1 in
      (* filter odd numbers *)
      if cur_num % 2 <> 0 then find_first next_num next_addend
      else
        let num_divisor = count_divisor cur_num in
          if num_divisor >= cnt then cur_num
          else find_first next_num next_addend
  in
    (* search from the first triangle number which is great then 1000 *)
    find_first 1035 (45 + 1)      (* triangle(45) = 1035 *)

printfn "%d" (solve_p12 500)
