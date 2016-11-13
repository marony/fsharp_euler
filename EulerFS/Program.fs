// F# の詳細については、http://fsharp.net を参照してください
// 詳細については、'F# チュートリアル' プロジェクトを参照してください。

open System.Diagnostics

(*
problem001 : answer = 233168, elaplsed = 00:00:00.0145570
problem002 : answer = 4613732L, elaplsed = 00:00:00.0277432
problem003 : answer = 6857L, elaplsed = 00:00:00.2178654
problem004 : answer = 906609L, elaplsed = 00:00:00.8837099
problem005 : answer = 232792560L, elaplsed = 00:00:00.0230161
problem006 : answer = 25164150, elaplsed = 00:00:00.0020924
problem007 : answer = 6863L, elaplsed = 00:00:00.0014047
problem008 : answer = 40824, elaplsed = 00:00:00.0080538
problem009 : answer = 31875000, elaplsed = 00:00:05.6479575
problem010 : answer = 142913828922L, elaplsed = 00:00:03.6394877
problem011 : answer = 70600674, elaplsed = 00:00:00.2449464
problem012 : answer = 76576500, elaplsed = 00:02:04.1292794
problem013 : answer = "5537376230", elaplsed = 00:00:00.0017144
problem014 : answer = (837799L, 524), elaplsed = 00:00:05.0099972
problem015 : answer = 137846528820, elaplsed = 00:00:00.0007773
problem060 : answer = 0, elaplsed = 00:00:00.0001829
[END]answer([8389L; 6733L; 5701L; 5197L; 13L]) = 26033
*)

[<EntryPoint>]
let main argv = 
    let watch = Stopwatch.StartNew()
    let answer = problems.problem001.problem001 1000
    printfn "problem001 : answer = %A, elaplsed = %A" answer watch.Elapsed

    let watch = Stopwatch.StartNew()
    let answer = problems.problem002.problem002 4000000L
    printfn "problem002 : answer = %A, elaplsed = %A" answer watch.Elapsed

    let watch = Stopwatch.StartNew()
    let answer = problems.problem003.problem003 600851475143L
    printfn "problem003 : answer = %A, elaplsed = %A" answer watch.Elapsed

    let watch = Stopwatch.StartNew()
    let answer = problems.problem004.problem004 3
    printfn "problem004 : answer = %A, elaplsed = %A" answer watch.Elapsed

    let watch = Stopwatch.StartNew()
    let answer = problems.problem005.problem005 20L
    printfn "problem005 : answer = %A, elaplsed = %A" answer watch.Elapsed

    let watch = Stopwatch.StartNew()
    let answer = problems.problem005.problem005_2 20L
    printfn "problem005_2 : answer = %A, elaplsed = %A" answer watch.Elapsed

    let watch = Stopwatch.StartNew()
    let answer = problems.problem006.problem006 1 100
    printfn "problem006 : answer = %A, elaplsed = %A" answer watch.Elapsed

    let watch = Stopwatch.StartNew()
    let answer = problems.problem007.problem007 10001
    printfn "problem007 : answer = %A, elaplsed = %A" answer watch.Elapsed

    let watch = Stopwatch.StartNew()
    let answer = problems.problem008.problem008 5
    printfn "problem008 : answer = %A, elaplsed = %A" answer watch.Elapsed

    (* 汚いので作り直したい *)
    let watch = Stopwatch.StartNew()
    let answer = problems.problem009.problem009 1000
    printfn "problem009 : answer = %A, elaplsed = %A" answer watch.Elapsed

    let watch = Stopwatch.StartNew()
    let answer = problems.problem010.problem010 2000000L
    printfn "problem010 : answer = %A, elaplsed = %A" answer watch.Elapsed

    let watch = Stopwatch.StartNew()
    let answer = problems.problem011.problem011 ()
    printfn "problem011 : answer = %A, elaplsed = %A" answer watch.Elapsed

    (* 遅い *)
    let watch = Stopwatch.StartNew()
    let answer = problems.problem012.problem012 500
    printfn "problem012 : answer = %A, elaplsed = %A" answer watch.Elapsed

    let watch = Stopwatch.StartNew()
    let answer = problems.problem013.problem013 ()
    printfn "problem013 : answer = %A, elaplsed = %A" answer watch.Elapsed

    let watch = Stopwatch.StartNew()
    let answer = problems.problem014.problem014 2L 1000000L
    printfn "problem014 : answer = %A, elaplsed = %A" answer watch.Elapsed

    let watch = Stopwatch.StartNew()
    let answer = problems.problem015.problem015 20
    printfn "problem015 : answer = %A, elaplsed = %A" answer watch.Elapsed

    let watch = Stopwatch.StartNew()
    let answer = problems.problem016.problem016 2I 1000I
    printfn "problem016 : answer = %A, elaplsed = %A" answer watch.Elapsed

    let watch = Stopwatch.StartNew()
    let answer = problems.problem018.problem018 ()
    printfn "problem018 : answer = %A, elaplsed = %A" answer watch.Elapsed

    let watch = Stopwatch.StartNew()
    let answer = problems.problem060.problem060 ()
    printfn "problem060 : answer = %A, elaplsed = %A" answer watch.Elapsed

    printfn "[END]"
    System.Console.ReadKey() |> ignore
    0
