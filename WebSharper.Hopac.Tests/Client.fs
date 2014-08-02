namespace WebSharper.Hopac.Tests

open Hopac
open IntelliFactory.WebSharper

[<JavaScript>]
module Client =

    let printfn x =
        JavaScript.Log x

    let Main =
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
                            return Alt.never ()
                        }
                    Alt.always 2
                ]
            return printfn ("SELECT: " + string x)
        }
        |> Job.Global.start
