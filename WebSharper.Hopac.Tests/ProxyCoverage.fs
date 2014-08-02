namespace WebSharper.Hopac.Tests

open Hopac
open IntelliFactory.WebSharper

[<JavaScript>]
module ProxyCoverage =

    open Job.Infixes

    let Jobs () =
        let a = Job.result 1
        let b = Job.result 2
        let c = Job.result ()
        job {
            do! Job.thunk (fun () -> ())
            do! Job.result 1 |> Job.start
            do! Job.unit ()
            do! a >>= fun x -> c
            do! a >>. c
            do! c .>> b
            do! a |>> ignore
            do! job.Combine(c, c)
            let! x = a
            for x in [1..3] do
                do! c
            while false do
                do! c
            do! job.Zero ()
            return! c
        }
        |> Job.Global.start

    open Alt.Infixes

    let Alts () =
        let a = Alt.always ()
        let b : Alt<unit> = Alt.never ()
        Alt.choose [
            Alt.always ()
            Alt.unit ()
            Alt.never ()
            Alt.zero ()
            Alt.guard (job { return Alt.always () })
            Alt.delay (fun () -> Alt.always ())
            Alt.withNack (fun nack -> Job.result (Alt.always ()))
            a <|> b
            a >>=? fun x -> Job.result ()
            a >>.? Job.result ()
            a .>>? Job.result ()
            a |>>? ignore
            a >>%? ()
        ]
        |> Alt.pick
        >>. Alt.select []

    let Chs () =
        let a : Ch<int> = Ch.Now.create ()
        Ch.Global.send a 0
        job {
            do!
                Alt.choose [
                    Ch.Alt.give a 1
                    Ch.Alt.take a >>%? ()
                ]
            do! Ch.give a 1
            let! _ = Ch.take a
            do! Ch.send a 1
        }
