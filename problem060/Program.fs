(*
Problem 60

The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes and concatenating
them in any order the result will always be prime. For example, taking 7 and 109, both 7109 and 1097
are prime. The sum of these four primes, 792, represents the lowest sum for a set of four primes with this property.
Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime.
*)

open System.Diagnostics
open Material.Utils
open Material.PrimeNumber

/// 数値の数
let numberOfPrime = 5

(*
4つの場合は3, 7, 109, 673

・2と5はあり得ない
・素数を小さな順で回して、2つで成り立つ、3つで成り立つ…で、5つの素数で成り立つのが結局最小の答え

answer(00:34:40.5591670) = [8389L; 6733L; 5701L; 5197L; 13L], 26033
*)

/// 2つの素数をくっつけても素数か返す
let checkPrime x y =
  isPrime (concatNumber x y) && isPrime (concatNumber y x)

/// 全ての素数を小さな順に処理する
let rec checkAllPrimes (watch : Stopwatch) (pss : seq<int64>) (xss : int64 list list) (minTotal : int64) =
  let minTotal2 = ref minTotal
  // 素数シーケンスの先頭
  let p = Seq.head pss
  // 最小の合計より大きな素数は処理しない
  if p < minTotal then
    // 2と5はあり得ない
    let xss' = if p <> 2L && p <> 5L then
                [ p ] :: [ for xs in xss do
                             if List.length xs < numberOfPrime 
                                && (List.forall (fun x -> checkPrime x p) xs)
                             then let r = p :: xs
                                  yield! [ xs; r ]
                                  if List.length r = numberOfPrime then
                                     let sum = List.sum r
                                     if !minTotal2 > sum then
                                       minTotal2 := sum
                                     printfn "answer(%A) = %A, %d" watch.Elapsed r sum
                             else yield xs ]
               else xss
    // 再帰呼び出し
    checkAllPrimes watch (Seq.skip 1 pss) xss' !minTotal2
  else xss

[<EntryPoint>]
let main argv =
  // 計測開始
  let watch = Stopwatch()
  watch.Start()
  // ロジック開始
  checkAllPrimes watch primes [] System.Int64.MaxValue |> ignore
  // 計測終了
  printfn "%A" watch.Elapsed
  printfn "[END]"
  System.Console.ReadKey () |> ignore
  0
