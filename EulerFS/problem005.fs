namespace problems

module problem005 =

  open Material.Math
  open Material.PrimeNumber
  
  /// maxを素因数分解し、素因数の重複を削除し、それらを掛けた数の倍数のうち1～max全てで割れる最小のもの
  let problem005 max = 

    let checkFactor (n : int64) (xs : int64 list) = 
      let rec checkFactor' (n : int64) (m : int64) (xs : int64 list) = 
        let n' = n * m
        if List.forall (fun x -> (n' % x) = 0L) xs then
          n'
        else
          checkFactor' n (m + 1L) xs

      checkFactor' n 1L xs

    let min = 1L
    max
    |> int
    |> factorial
    |> int64
    |> primeFactors
    |> Seq.distinct
    |> Seq.fold (*) 1L
    |> (fun x -> checkFactor x [ min..max ])

  /// 1～maxをそれぞれ素因数分解し、素因数の数が一番多いものを残して削除して、残った素因数を掛け合わせる
  let problem005_2 max =
    let addMap map (n, c) =
      if Map.containsKey n map then
        let old = Map.find n map
        Map.add n (Operators.max old c) map
      else
        Map.add n c map
    // [[1L]; [2L]; [3L]; [2L; 2L]; [5L]; [3L; 2L]; [7L]; [2L; 2L; 2L]; [3L; 3L];
    // [5L; 2L]; [11L]; [3L; 2L; 2L]; [13L]; [7L; 2L]; [5L; 3L]; [2L; 2L; 2L; 2L];
    // [17L]; [3L; 3L; 2L]; [19L]; [5L; 2L; 2L]]
    let min = 1L
    [min..max]
    // problem005_2 : answer = [seq [(1L, 1)]; seq [(2L, 1)]; seq [(3L, 1)]; seq [(2L, 2)]; seq [(5L, 1)];
    // seq [(3L, 1); (2L, 1)]; seq [(7L, 1)]; seq [(2L, 3)]; seq [(3L, 2)];
    // seq [(5L, 1); (2L, 1)]; seq [(11L, 1)]; seq [(3L, 1); (2L, 2)]; seq [(13L, 1)];
    // seq [(7L, 1); (2L, 1)]; seq [(5L, 1); (3L, 1)]; seq [(2L, 4)]; seq [(17L, 1)];
    // seq [(3L, 2); (2L, 1)]; seq [(19L, 1)]; seq [(5L, 1); (2L, 2)]], elaplsed = 00:00:00.0060024
    |> List.map (primeFactors >> Seq.countBy (fun n -> n))
    // problem005_2 : answer = [(1L, 1); (2L, 1); (3L, 1); (2L, 2); (5L, 1); (3L, 1); (2L, 1); (7L, 1); (2L, 3);
    // (3L, 2); (5L, 1); (2L, 1); (11L, 1); (3L, 1); (2L, 2); (13L, 1); (7L, 1);
    // (2L, 1); (5L, 1); (3L, 1); (2L, 4); (17L, 1); (3L, 2); (2L, 1); (19L, 1);
    // (5L, 1); (2L, 2)], elaplsed = 00:00:00.0209326
    |> Seq.concat
    // problem005_2 : answer = map
    // [(1L, 1); (2L, 4); (3L, 2); (5L, 1); (7L, 1); (11L, 1); (13L, 1); (17L, 1);
    // (19L, 1)], elaplsed = 00:00:00.0432262
    |> Seq.fold addMap Map.empty
    // 232792560L
    |> Map.fold (fun acc n c -> acc * (pown n c)) 1L
