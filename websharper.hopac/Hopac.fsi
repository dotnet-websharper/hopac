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

[<AbstractClass>]
type Job<'T> =
    abstract internal Run : ('T -> unit) -> unit

module Job =
    module Global  =
        val start : Job<'T> -> unit

    val thunk : (unit -> 'T) -> Job<'T>
    val unit : unit -> Job<unit>
    val result : 'T -> Job<'T>

    module Infixes =
        val ( >>= ) : Job<'A> -> ('A -> Job<'B>) -> Job<'B>
        val ( >>. ) : Job<'A> -> Job<'B> -> Job<'B>
        val ( .>> ) : Job<'A> -> Job<'B> -> Job<'A>
        val ( |>> ) : Job<'A> -> ('A -> 'B) -> Job<'B>

type JobBuilder

type JobBuilder with
    new : unit -> JobBuilder
    member Bind : Job<'A> * ('A -> Job<'B>) -> Job<'B>
    member Combine : Job<unit> * Job<'T> -> Job<'T>
    member Delay : (unit -> Job<'T>) -> Job<'T>
    member For : seq<'T> * ('T -> Job<unit>) -> Job<unit>
    member For : array<'T> * ('T -> Job<unit>) -> Job<unit>
    member Return : 'T -> Job<'T>
    member ReturnFrom : Job<'T> -> Job<'T>
    member While : (unit -> bool) * Job<unit> -> Job<unit>
    member Zero : unit -> Job<unit>

type internal Branch<'T>

[<AbstractClass>]
type Alt<'T> =
    inherit Job<'T>
    abstract internal Init : selected: Alt<int> -> pos: int -> Job<Branch<'T>[]>

[<Sealed>]
type IVar<'T> =
    class inherit Alt<'T> end

module Alt =
    val always : 'T -> Alt<'T>
    val unit : unit -> Alt<unit>
    val never : unit -> Alt<'T>
    val zero : unit -> Alt<unit>
    val guard : Job<Alt<'T>> -> Alt<'T>
    val delay : (unit -> Alt<'T>) -> Alt<'T>
    val withNack : (Alt<unit> -> Job<Alt<'T>>) -> Alt<'T>
    val choose : seq<Alt<'T>> -> Alt<'T>
    module Infixes =
        val ( <|> ) : Alt<'T> -> Alt<'T> -> Alt<'T>
        val ( >>=? ) : Alt<'A> -> ('A -> Job<'B>) -> Alt<'B>
        val ( >>.? ) : Alt<_> -> Job<'T> -> Alt<'T>
        val ( .>>? ) : Alt<'T> -> Job<_> -> Alt<'T>
        val ( |>>? ) : Alt<'A> -> ('A -> 'B) -> Alt<'B>
        val ( >>%? ) : Alt<'A> -> 'B -> Alt<'B>
//        val ( >>!? ) : Alt<'T> -> exn -> Alt<_>
//    val tryIn : Alt<'T> -> ('T -> Job<'B>) -> (exn -> Job<'B>) -> Alt<'B>
    val pick : Alt<'T> -> Job<'T>
    val select : seq<Alt<'T>> -> Job<'T>

type Ch<'T> =
    class inherit Alt<'T> end

module Ch  =
    module Now =
        val create : unit -> Ch<'T>
    module Alt =
        val give : Ch<'T> -> 'T -> Alt<unit>
        val take : Ch<'T> -> Alt<'T>
    module Global =
        val send : Ch<'T> -> 'T -> unit
    val create : unit -> Job<Ch<'T>>
    val give : Ch<'T> -> 'T -> Job<unit>
    val send : Ch<'T> -> 'T -> Job<unit>
    val take : Ch<'T> -> Job<'T>

[<AutoOpen>]
module TopLevel =
    val job : JobBuilder
