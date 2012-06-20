module Util

(* List all prime numbers less than `max_num'.
 * primes 20 => [19; 17; 13; 11; 7; 5; 3; 2]
 * ----------------------------------------------------------------
 *)
let primes max_num =
  let filter_with n lst =
    let cnt = ref 0 in
    let rst =
      List.filter
        (fun i -> if i % n <> 0 then true; else (cnt := !cnt + 1; false))
        lst
    in
      (!cnt, rst)

  let rec get_all_primes filter_nums nums =
    if nums = [] then filter_nums
    else
      let fnum = List.head nums in
      let (cnt, new_nums) = filter_with fnum (List.tail nums) in
      let new_fileter_nums = fnum :: filter_nums in
        if cnt < 2 then (List.rev new_nums) @ new_fileter_nums
        else get_all_primes new_fileter_nums new_nums
  in
    get_all_primes [] [2 .. max_num]

let max_prod_of_adjacent (step: int) (digit_array: int []) =
  let prod step idx =
    let max_idx = idx + step - 1 in
      Array.fold (fun acc n -> acc * n) 1 (digit_array.[idx .. max_idx]) in

  let rec scan_array acc scan_idx max_idx =
    if scan_idx > max_idx then acc
    else
      if digit_array.[scan_idx] = 0 then scan_array acc (scan_idx + step) max_idx
      else
        let new_acc = prod step scan_idx in
        let new_idx = scan_idx + 1 in
          if acc < new_acc then scan_array new_acc new_idx max_idx
          else scan_array acc new_idx max_idx
  in
    if Array.length digit_array < step then 0
    else scan_array (prod step 0) 1 (Array.length digit_array - step)
