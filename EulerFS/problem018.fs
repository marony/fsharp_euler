namespace problems

(*
ex)
1
2 3
4 5 6

ロジック)
1. 一番下の行を端っこを除いて複製する
[4; 5; 6] → [4; 5; 5; 6]

2. 下から二番目の行を複製する
[2; 3] → [2; 2; 3; 3]

3. 縦方向に足す
[6; 7; 8; 9]

4. ペアごとに大きいほうだけ残す
[7; 9]

5. 次の行に進む
1と同じ処理 ※ 両方端っこなので複製されない
[7; 9]

2と同じ処理
[1; 1]

3と同じ処理
[8; 10]

4と同じ処理
[10]

これが答え！！！
*)
module problem018 =

  let data = [[75;];
              [95; 64;];
              [17; 47; 82;];
              [18; 35; 87; 10;];
              [20; 04; 82; 47; 65;];
              [19; 01; 23; 75; 03; 34;];
              [88; 02; 77; 73; 07; 63; 67;];
              [99; 65; 04; 28; 06; 16; 70; 92;];
              [41; 41; 26; 56; 83; 40; 80; 70; 33;];
              [41; 48; 72; 33; 47; 32; 37; 16; 94; 29;];
              [53; 71; 44; 65; 25; 43; 91; 52; 97; 51; 14;];
              [70; 11; 33; 28; 77; 73; 17; 78; 39; 68; 17; 57;];
              [91; 71; 52; 38; 17; 14; 91; 43; 58; 50; 27; 29; 48;];
              [63; 66; 04; 68; 89; 53; 67; 30; 73; 16; 69; 87; 40; 31;];
              [04; 62; 98; 27; 23; 09; 70; 98; 73; 93; 38; 53; 60; 04; 23;]]

  /// 縦方向(y)・横方向(x)のインデックスを付与する
  /// 縦方向のインデックスは結局使ってないけど
  let makeIndex =
    let makeIndex' y line =
      let xs = List.mapi (fun x n -> ((x, y), n)) line
      (List.length xs, xs)
    List.mapi (fun y line -> makeIndex' y line)

  /// 全項目をそれぞれ複製する
  /// [1; 2; 3] → [1; 1; 2; 2; 3; 3]
  let duplicate1 xs =
    xs
    |> List.map (fun x -> [x; x])
    |> List.concat
    
  /// 全項目をそれぞれ複製する(端っこ以外)
  /// [1; 2; 3] → [1; 2; 2; 3]
  let duplicate2 xs =
    xs
    |> List.mapi (fun i x ->
      if i = 0 || i + 1 >= (List.length xs) then
        [x]
      else
        [x; x])
    |> List.concat

  /// 項目を足し算する
  let sumList (((a, b), xs), ((c, d), ys)) =
    ((c, d), xs + ys)

  /// ペアの値の大きい方を採用する
  let selectList (i, xs) =
    let xs = Seq.toList xs
    match xs with
    | ((a, b), x) :: ((c, d), y) :: [] ->
      if x > y then ((a, b), x) else ((c, d), y)
    | _ -> ((-1, -1), -1) // あり得ない(エラー)

  /// 実際のロジック
  let rec processAll xss =
    match xss with
    | (l, xs) :: (m, ys) :: tail ->
      let xs = duplicate2 xs
      let ys = duplicate1 ys
      let rs =
        List.zip xs ys
        |> List.map sumList
        |> Seq.groupBy (fun ((a, b), xs) -> a) 
        |> Seq.toList
        |> List.map selectList
      processAll ((m, rs) :: tail)
    | xs :: [] -> xss
    | [] -> []

  let problem018 () =
    data
    |> List.rev
    |> makeIndex
    |> processAll
