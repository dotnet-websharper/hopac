module WebSharper.Hopac.Main

module Reference =
    open Hopac

    let tee (input: Ch<'A>) (out1: Ch<'A>) (out2: Ch<'A>) : Job<unit> =
        job {
            while true do
                let! v = Ch.take input
                do! Ch.give out1 v
                do! Ch.give out2 v
        }
        |> Job.start

    type T<'A,'B> = Ch<'A> -> Ch<'B> -> Job<unit>

    let fold (zero: 'R) (f: 'T -> 'R -> 'R) : T<'T,'R> =
        fun input output ->
            job {
                let cur = ref zero
                while true do
                    do! Ch.give output !cur
                    let! i = Ch.take input
                    do cur := f i !cur
            }
            |> Job.start

    let map (f: 'A -> 'B) : T<'A,'B> =
        fun input output ->
            job {
                while true do
                    let! v = Ch.take input
                    do! Ch.give output (f v)
            }
            |> Job.start

    let compose (t1: T<'A,'B>) (t2: T<'B,'C>) : T<'A,'C> =
        fun input output ->
            job {
                let! c = Ch.create ()
                do! t1 input c
                do! t2 c output
            }

    let push (out: Ch<'T>) (xs: seq<'T>) : Job<unit> =
        job {
            for x in xs do
                do! Ch.give out x
        }
        |> Job.start

    let print (prefix: string) (input: Ch<int>) : Job<unit> =
        job {
            while true do
                let! i = Ch.take input
                do printfn "%s: %i" prefix i
        }
        |> Job.start

    let test () =
        job {
            let! input = Ch.create ()
            let! output = Ch.create ()
            let! out1 = Ch.create ()
            let! out2 = Ch.create ()

            let incr x = x + 1
            let ( ++ ) x y = compose x y
            let tr = map incr ++ map incr ++ fold 0 (+)
            do! push input [1..5]
            do! tee input out1 out2
            do! tr out1 output
            do! print "OUTPUT" output
            do! print "OUT2" out2
        }
        |> Job.Global.start

module Actual =
    open WebSharper.Hopac

    let tee (input: Ch<'A>) (out1: Ch<'A>) (out2: Ch<'A>) : Job<unit> =
        job {
            while true do
                let! v = Ch.take input
                do! Ch.give out1 v
                do! Ch.give out2 v
        }
        |> Job.start

    type T<'A,'B> = Ch<'A> -> Ch<'B> -> Job<unit>

    let fold (zero: 'R) (f: 'T -> 'R -> 'R) : T<'T,'R> =
        fun input output ->
            job {
                let cur = ref zero
                while true do
                    do! Ch.give output !cur
                    let! i = Ch.take input
                    do cur := f i !cur
            }
            |> Job.start

    let map (f: 'A -> 'B) : T<'A,'B> =
        fun input output ->
            job {
                while true do
                    let! v = Ch.take input
                    do! Ch.give output (f v)
            }
            |> Job.start

    let compose (t1: T<'A,'B>) (t2: T<'B,'C>) : T<'A,'C> =
        fun input output ->
            job {
                let! c = Ch.create ()
                do! t1 input c
                do! t2 c output
            }

    let push (out: Ch<'T>) (xs: seq<'T>) : Job<unit> =
        job {
            for x in xs do
                do! Ch.give out x
        }
        |> Job.start

    let print (prefix: string) (input: Ch<int>) : Job<unit> =
        job {
            while true do
                let! i = Ch.take input
                do printfn "%s: %i" prefix i
        }
        |> Job.start

    open Alt.Infixes

    let test () =
        job {
            let! x =
                Alt.select [
                    Alt.withNack <| fun nack ->
                        job {
                            do!
                                job {
                                    do! nack
                                    return printfn "NACK!"
                                }
                                |> Job.start
                            do printfn "ignoring nack"
                            return Alt.always 1
                        }
                    Alt.always 1
                ]

//                    Alt.always () >>=? fun () ->
//                        printfn "V2"
//                        Job.result 2
//                ]
            return printfn "SELECT: %i" x
        }
        |> Job.Global.start

[<EntryPoint>]
let start args =
//    stdout.WriteLine("Test Reference")
//    Reference.test ()
//    stdin.ReadLine ()
//    |> ignore
    stdout.WriteLine("Test Actual")
    Actual.test ()
    stdin.ReadLine ()
    |> ignore
    0
