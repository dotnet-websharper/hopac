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
module internal Util =

    let U<'T> = Unchecked.defaultof<'T>

    module Array =

        let balancedReduce f zero arr =
            let len = Array.length arr
            match len with
            | 0 -> zero
            | 1 -> arr.[0]
            | _ ->
                let rec loop off len =
                    if len = 1 then arr.[off] else
                        let l2 = len / 2
                        f (loop off l2) (loop (off + l2) (len - l2))
                loop 0 len
