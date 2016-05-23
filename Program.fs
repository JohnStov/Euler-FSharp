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

allTriples 1 1000; |> Seq.filter isPythagorean |> Seq.find (sumsTo 1000) |> product

//Problem 10; - See problem 3
let bigSeqSum (x : int seq) =
    x |> Seq.fold (fun acc elem -> acc + (bigint elem)) 0I

allPrimes |> Seq.takeWhile (fun x -> x < 2000000) |> bigSeqSum

// problem 11
let grid = 
    [[08; 02; 22; 97; 38; 15; 00; 40; 00; 75; 04; 05; 07; 78; 52; 12; 50; 77; 91; 08];
     [49; 49; 99; 40; 17; 81; 18; 57; 60; 87; 17; 40; 98; 43; 69; 48; 04; 56; 62; 00];
     [81; 49; 31; 73; 55; 79; 14; 29; 93; 71; 40; 67; 53; 88; 30; 03; 49; 13; 36; 65];
     [52; 70; 95; 23; 04; 60; 11; 42; 69; 24; 68; 56; 01; 32; 56; 71; 37; 02; 36; 91];
     [22; 31; 16; 71; 51; 67; 63; 89; 41; 92; 36; 54; 22; 40; 40; 28; 66; 33; 13; 80];
     [24; 47; 32; 60; 99; 03; 45; 02; 44; 75; 33; 53; 78; 36; 84; 20; 35; 17; 12; 50];
     [32; 98; 81; 28; 64; 23; 67; 10; 26; 38; 40; 67; 59; 54; 70; 66; 18; 38; 64; 70];
     [67; 26; 20; 68; 02; 62; 12; 20; 95; 63; 94; 39; 63; 08; 40; 91; 66; 49; 94; 21];
     [24; 55; 58; 05; 66; 73; 99; 26; 97; 17; 78; 78; 96; 83; 14; 88; 34; 89; 63; 72];
     [21; 36; 23; 09; 75; 00; 76; 44; 20; 45; 35; 14; 00; 61; 33; 97; 34; 31; 33; 95];
     [78; 17; 53; 28; 22; 75; 31; 67; 15; 94; 03; 80; 04; 62; 16; 14; 09; 53; 56; 92];
     [16; 39; 05; 42; 96; 35; 31; 47; 55; 58; 88; 24; 00; 17; 54; 24; 36; 29; 85; 57];
     [86; 56; 00; 48; 35; 71; 89; 07; 05; 44; 44; 37; 44; 60; 21; 58; 51; 54; 17; 58];
     [19; 80; 81; 68; 05; 94; 47; 69; 28; 73; 92; 13; 86; 52; 17; 77; 04; 89; 55; 40];
     [04; 52; 08; 83; 97; 35; 99; 16; 07; 97; 57; 32; 16; 26; 26; 79; 33; 27; 98; 66];
     [88; 36; 68; 87; 57; 62; 20; 72; 03; 46; 33; 67; 46; 55; 12; 32; 63; 93; 53; 69];
     [04; 42; 16; 73; 38; 25; 39; 11; 24; 94; 72; 18; 08; 46; 29; 32; 40; 62; 76; 36];
     [20; 69; 36; 41; 72; 30; 23; 88; 34; 62; 99; 69; 82; 67; 59; 85; 74; 04; 36; 16];
     [20; 73; 35; 29; 78; 31; 90; 01; 74; 31; 49; 71; 48; 86; 81; 16; 23; 57; 05; 54];
     [01; 70; 54; 71; 83; 51; 54; 69; 16; 92; 33; 48; 61; 43; 52; 01; 89; 19; 67; 48]]

let horizontals = 
    seq { for i in 0..19 do for j in 0..16 do yield grid.[i].[j..(j+3)] }

let verticals = 
    seq { for i in 0..16 do for j in 0..19 do yield [grid.[i].[j];grid.[i+1].[j];grid.[i+2].[j];grid.[i+3].[j]] }

let right_diags =
    seq { for i in 0..16 do for j in 0..16 do yield [grid.[i].[j];grid.[i+1].[j+1];grid.[i+2].[j+2];grid.[i+3].[j+3]] }

let left_diags =
    seq { for i in 0..16 do for j in 0..16 do yield [grid.[i].[j+3];grid.[i+1].[j+2];grid.[i+2].[j+1];grid.[i+3].[j]] }

let allGroups = 
    seq { 
        yield! horizontals
        yield! verticals
        yield! right_diags
        yield! left_diags }

let product x = 
    x |> Seq.fold (fun acc elem -> acc * elem) 1

allGroups |> Seq.map (fun elem -> elem |> product) |> Seq.max

