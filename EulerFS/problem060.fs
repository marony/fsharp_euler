namespace problems

module problem060 =

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
  let rec checkAllPrimes (pss : seq<int64>) (xss : int64 list list) (minTotal : int64) =
    let minTotal2 = ref minTotal
    // 素数シーケンスの先頭
    let p = Seq.head pss
    // 最小の合計より大きな素数は処理しない
    if p < minTotal then
      // 2と5はあり得ない
      let xss' =
        if p <> 2L && p <> 5L then
          [ p ] :: [
            for xs in xss do
              if List.length xs < numberOfPrime &&
                (List.forall (checkPrime p) xs) then
                let r = p :: xs
                yield! [ xs; r ]
                if List.length r = numberOfPrime then
                  let sum = List.sum r
                  if !minTotal2 > sum then minTotal2 := sum
                  printfn "answer(%A) = %d" r sum
              else yield xs
            ]
        else xss
      // 再帰呼び出し
      checkAllPrimes (Seq.skip 1 pss) xss' !minTotal2
    else xss

  let problem060 () =
    // ロジック開始
    checkAllPrimes primes [] System.Int64.MaxValue
