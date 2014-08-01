// $begin{copyright}
//
// This file is confidential and proprietary.
//
// Copyright (c) IntelliFactory, 2004-2014.
//
// All rights reserved.  Reproduction or use in whole or in part is
// prohibited without the written consent of the copyright holder.
//-----------------------------------------------------------------
// $end{copyright}

namespace WebSharper.Hopac

open IntelliFactory.WebSharper

[<AutoOpen>]
[<JavaScript>]
module internal Executor =

    type private Work = unit -> unit

//    let private agent =
//        MailboxProcessor<Work>.Start (fun agent ->
//            async {
//                while true do
//                    let! work = agent.Receive ()
//                    do work ()
//            })
//
//    let spawn work =
//        agent.Post work

    // For now, simply reusing Async scheduler.
    //
    // TODO: can do better, in JS?
    //
    // Requirements are quite similar to Async scheduler:
    //      (1) periodically drop control to setImmediate/setTimeout
    //      (2) interleave "threads"
    //      (3) do something similar to Hopac about exceptions
    //      (4) trampolining - well, (1) should do trampolining
    let spawn (work: Work) =
        async { return work () }
        |> Async.Start
