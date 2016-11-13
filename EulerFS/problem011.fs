namespace problems

module problem011 =

  let data = "\
08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08 \
49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00 \
81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65 \
52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91 \
22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80 \
24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50 \
32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70 \
67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21 \
24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72 \
21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95 \
78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92 \
16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57 \
86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58 \
19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40 \
04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66 \
88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69 \
04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36 \
20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16 \
20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54 \
01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48\
"
  
  /// データの縦横の長さ
  let len = 20
  /// 数字の数
  let cnt = 4

  /// 座標からインデックス
  let indexFromPos x y =
    y * len + x

  /// インデックスから座標
  let posFromIndex index =
    (index % len), (index / len)

  /// 方向
  type Direction =
  | North
  | NorthEast
  | East
  | SouthEast
  | South
  | SouthWest
  | West
  | NorthWest
    static member all = [North; NorthEast; East; SouthEast; South; SouthWest; West; NorthWest]

  /// チェック関数を返す
  let checkFunc dir =
    // 上に行けるかチェック
    let checkNorth index =
      let x, y = posFromIndex index
      y > 0
    /// 右に行けるかチェック
    let checkEast index =
      let x, y = posFromIndex index
      x < len - 1
    /// 下に行けるかチェック
    let checkSouth index =
      let x, y = posFromIndex index
      y < len - 1
    /// 左に行けるかチェック    
    let checkWest index =
      let x, y = posFromIndex index
      x > 0

    match dir with
    | North -> checkNorth
    | NorthEast -> (fun index -> checkNorth index && checkEast index)
    | East -> checkEast
    | SouthEast -> (fun index -> checkEast index && checkSouth index)
    | South -> checkSouth
    | SouthWest -> (fun index -> checkSouth index && checkWest index)
    | West -> checkWest
    | NorthWest -> (fun index -> checkWest index && checkNorth index)

  /// 移動関数を返す
  let moveFunc dir =
    /// 上に行く
    let north index =
      let x, y = posFromIndex index
      index - len
    /// 右に行く
    let east index =
      index + 1
    /// 下に行く
    let south index =
      index + len
    /// 左に行く
    let west index =
      index - 1

    match dir with
    | North -> north
    | NorthEast -> north >> east
    | East -> east
    | SouthEast -> east >> south
    | South -> south
    | SouthWest -> south >> west
    | West -> west
    | NorthWest -> west >> north

  /// あるインデックスから8方向にcnt分進んだリストを返す
  let makeList index =
    [for dir in Direction.all do
      let check = checkFunc dir
      let move = moveFunc dir
      let rec makeList' index xs =
        if List.length xs >= cnt || not (check index) then
          xs
        else
          let index' = move index
          makeList' index' (index' :: xs)
      yield makeList' index [index]]

  let problem011 () =
    // 数値の配列に変換する
    let d =
      data.Split(' ', '\r')
      |> Seq.map (fun s -> int s)
      |> Seq.toArray
    [0..Array.length d - 1]
    |> List.map makeList
    |> List.concat
    |> List.filter (fun xs -> List.length xs = cnt)
    |> List.map (fun xs -> List.fold (fun acc x -> acc * d.[x]) 1 xs)
    |> List.max
