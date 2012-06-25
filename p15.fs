let solve_p15 (num: int) =
  let grid =
    let n = num + 1 in
      Array2D.init n n (fun i j -> if i = 0 || j = 0 then 1L else 0L)
  in
  let fill_grid (g: int64 [,]) =
    let idx = [1 .. num] in
      List.iter (fun y ->
                   List.iter (fun x ->
                                g.[y, x] <- g.[y, x-1] + g.[y-1, x]) idx) idx
  in
    fill_grid grid; grid.[num, num]


printfn "%d" (solve_p15 20)
