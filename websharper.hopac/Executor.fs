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

[<AutoOpen>]
module internal Executor =

    type private Work = unit -> unit

    let private agent =
        MailboxProcessor<Work>.Start (fun agent ->
            async {
                while true do
                    let! work = agent.Receive ()
                    do work ()
            })

    let spawn work =
        agent.Post work
