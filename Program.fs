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
let isPrime n =
  let sqrt' = (float >> sqrt >> int) n // square root of integer
  [ 2 .. sqrt' ] // all numbers from 2 to sqrt'
  |> List.forall (fun x -> n % x <> 0) // no divisors

let allPrimes =
  let rec allPrimes' n =
    seq { // sequences are lazy, so we can make them infinite
      if isPrime n then
        yield n
      yield! allPrimes' (n+1) // recursing
    }
  allPrimes' 2 // starting from 2

let factors (n : bigint) = 
    allPrimes |> Seq.takeWhile (fun x -> x < int (sqrt (float n))) |> Seq.filter (fun x -> n % bigint x = 0I) |> List.ofSeq 

factors 600851475143I |> List.last

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

// Problem 6
let sumSquares l =
    l |> List.map (fun x -> x * x) |> List.sum

let squareSum l =
    l |> List.sum |> (fun x -> x * x) 

let nums = [1..100]
    
(nums |> squareSum) - (nums |> sumSquares) Seq.l

// Problem 7 - see problem 3
allPrimes |> Seq.take 10001 |> List.ofSeq |> List.last;;