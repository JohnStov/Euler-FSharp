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

// Problem 8
let digits =  
    "73167176531330624919225119674426574742355349194934"+
    "96983520312774506326239578318016984801869478851843"+
    "85861560789112949495459501737958331952853208805511"+
    "12540698747158523863050715693290963295227443043557"+
    "66896648950445244523161731856403098711121722383113"+
    "62229893423380308135336276614282806444486645238749"+
    "30358907296290491560440772390713810515859307960866"+
    "70172427121883998797908792274921901699720888093776"+
    "65727333001053367881220235421809751254540594752243"+
    "52584907711670556013604839586446706324415722155397"+
    "53697817977846174064955149290862569321978468622482"+
    "83972241375657056057490261407972968652414535100474"+
    "82166370484403199890008895243450658541227588666881"+
    "16427171479924442928230863465674813919123162824586"+
    "17866458359124566529476545682848912883142607690042"+
    "24219022671055626321111109370544217506941658960408"+
    "07198403850962455444362981230987879927244284909188"+
    "84580156166097919133875499200524063689912560717606"+
    "05886116467109405077541002256983155200055935729725"+
    "71636269561882670428252483600823257530420752963450"

let charValue c =
    c.ToString() |> bigint.Parse

let seqProduct (s : char seq) =
    s |> Seq.fold (fun acc elem -> (charValue elem) * acc) 1I

digits :> seq<char> |> Seq.windowed 13 |> Seq.map seqProduct |> Seq.max

// Problem 9
let allDoubles n limit =
    seq {
        for i in (n+1) .. limit do yield (n, i)
    }  

let prefix n t =
    let x, y = t
    (n, x, y)

let allTriples n limit =
    seq {
        for i in n .. limit do 
            for j in (i + 1) .. limit do 
                for t in allDoubles j limit do yield prefix i t
    }  

let isPythagorean t =
    let a, b, c = t
    (a * a) + (b * b) = (c * c)

let sumsTo n t =
    let a, b, c = t
    a + b + c = n

let product t =
    let a, b, c = t
    a * b * c    

allTriples 1 1000 |> Seq.filter isPythagorean |> Seq.find (sumsTo 1000) |> product

