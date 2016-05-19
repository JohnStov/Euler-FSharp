// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

// Problem 1
[1..999] |> List.filter (fun x -> x % 3 = 0 || x % 5 = 0) |> List.sum


// Problem 2
let rec fib n =
    match n with
    | 0 -> 1
    | 1 -> 1
    | _ -> fib (n - 2) + fib (n - 1)

let fibs = Seq.initInfinite (fun n -> fib n)
fibs |> Seq.takeWhile(fun x -> x <= 4000000) |> Seq.filter (fun x -> x % 2 = 0) |> Seq.sum

