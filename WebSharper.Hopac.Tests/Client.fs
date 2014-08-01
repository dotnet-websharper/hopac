namespace WebSharper.Hopac.Tests

open Hopac
open IntelliFactory.WebSharper

[<JavaScript>]
module Client =

    let Main =
        job {
            return JavaScript.Log("JOB RUNNING")
        }
        |> Job.Global.start

