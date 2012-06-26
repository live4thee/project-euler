let solve_p16 (num: int) =
  let c2n ch = int ch - int '0' in
  let s = (pown 2I num).ToString() in
    Seq.map c2n s |> Seq.sum

printfn "%d" (solve_p16 1000)
