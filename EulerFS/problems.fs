namespace problems

module problem001 = 
  let problem001 max = 
    Seq.sum [ for n in 1..(max - 1) do
                if (n % 3) = 0 || (n % 5) = 0 then yield n ]

module problem002 = 
  open Material.Math
  
  let problem002 max = 
    Seq.takeWhile (fun n -> n <= max) fibonacci
    |> Seq.filter (fun n -> (n % 2) = 0)
    |> Seq.sum

module problem003 = 
  open Material.PrimeNumber
  
  let problem003 n = primeFactors n |> List.max

module problem004 = 
  open Material.Utils
  
  let problem004 n = 
    let min = pown 10 (n - 1)
    let max = (pown 10 n) - 1
    [ for x in [ min..max ] do
        for y in [ min..max ] -> x * y ]
    |> List.map int64
    |> List.filter isPalindrome
    |> List.max

module problem006 = 
  let problem006 min max = 
    let sumOfSquares = 
      [ min..max ]
      |> List.map (fun n -> pown n 2)
      |> List.sum
    
    let squareOfSum = 
      [ min..max ]
      |> List.sum
      |> (fun n -> pown n 2)
    
    squareOfSum - sumOfSquares

module problem007 = 
  open Material.PrimeNumber
  
  let problem007 n = Seq.nth (n - 1) primes

module problem009 = 
  let problem009 max = 
    Seq.head [ for a in [ 1..max - 2 ] do
                 for b in [ a + 1..max - 1 - a ] do
                   if a * a + b * b <= max * max then 
                     for c in [ b + 1..max - a - b ] do
                       if (a * a + b * b = c * c) && (a + b + c = 1000) then yield a * b * c ]

module problem010 = 
  open Material.PrimeNumber
  
  let problem010 max = 
    primes' max
    |> Seq.takeWhile (fun n -> n < max)
    |> Seq.sum

module problem012 = 
  open Material.Math
  open Material.PrimeNumber
  
  (*
n = 12000L, t = 72006000L
f(160) = [1L; 2L; 3L; 4L; 5L; 6L; 8L; 10L; 11L; 12L; 15L; 16L; 20L; 22L; 24L; 25L; 30L;
 33L; 40L; 44L; 48L; 50L; 55L; 60L; 66L; 75L; 80L; 88L; 100L; 110L; 120L; 125L;
 132L; 150L; 165L; 176L; 200L; 220L; 240L; 250L; 264L; 275L; 300L; 330L; 375L;
 400L; 440L; 500L; 528L; 550L; 600L; 660L; 750L; 825L; 880L; 1000L; 1091L; 1100L;
 1200L; 1320L; 1375L; 1500L; 1650L; 2000L; 2182L; 2200L; 2640L; 2750L; 3000L;
 3273L; 3300L; 4125L; 4364L; 4400L; 5455L; 5500L; 6000L; 6546L; 6600L; 8250L;
 8728L; 10910L; 11000L; 12001L; 13092L; 13200L; 16365L; 16500L; 17456L; 21820L;
 22000L; 24002L; 26184L; 27275L; 32730L; 33000L; 36003L; 43640L; 48004L; 52368L;
 ...]
p = [1091L; 11L; 5L; 5L; 5L; 3L; 2L; 2L; 2L; 2L]
r = 160
n = 12375L
76576500L
elapsed = 00:05:02.8421057
*)

  let triangle (n : int) = 
    let t = triangleNumber n |> int64
    let p = primeFactors (int64 t)
    
    let r = 
      p
      |> Seq.countBy (fun n -> n)
      |> Seq.map (fun (a, b) -> (int a, b + 1))
      |> Seq.reduce (fun (a, b) (c, d) -> (n, b * d))
      |> snd
    r
  
  let problem012 max = 
    Seq.initInfinite (fun i -> i + 1)
    |> Seq.find (fun n -> (triangle n) > max)
    |> triangleNumber

module problem014 =
  let processOne n =
    let rec process' n c =
      let n' =
        if (n % 2L) = 0L then
          n / 2L
        else
          3L * n + 1L
      if n' <= 1L then c
      else process' n' (c + 1)
    process' n 1

  let problem014 min max =
    [min..max]
    |> List.map processOne
    |> List.zip [min..max]
    |> List.maxBy (fun (a, b) -> b)

module problem015 =
  open Material.Math

(*
20x20のマス目を左上から右下まで→と↓で行くには
20回→に進んで、20回↓に進むの組み合わせ

なので、40の中から20を取る組み合わせ
*)

  let problem015 n = nCr (n * 2) n

// problem016 : answer = 10715086071862673209484250490600018105614048117055336074437503883703510511249361224931983788156958
// 581275946729175531468251871452856923140435984577574698574803934567774824230985421074605062371141877954182153046474983581
// 941267398767559165543946077062914571196477686542167660429831652624386837205668069376, elaplsed = 00:00:00.1877294
module problem016 =
  open Material.Math

  let problem016 n m =
    powb n m
    |> string
    |> Seq.map (fun c -> (int c) - (int '0'))
    |> Seq.toList
    |> List.sum
