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


// Problem 3

// Problem 4
let reverse (s : string) = 
    System.String(s |> Seq.rev |> Seq.toArray)

let isPalindrome n =
    let s = n.ToString()
    s = reverse s

[for x in [100..999] do for y in [100..999] do yield (x * y)] |> List.filter isPalindrome |> List.max

// Problem 5
let isEvenlyDivisibleBy nums n =
    nums |> List.forall (fun x -> n % x = 0)

let rec allInts n = seq {
    yield n
    yield! allInts (n + 1)
    }

allInts 1 |> Seq.find (isEvenlyDivisibleBy ([2..20] |> List.rev)) |> int

