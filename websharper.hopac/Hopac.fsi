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

type internal A<'T> = Hopac.Alt<'T>
type internal C<'T> = Hopac.Ch<'T>
type internal J<'T> = Hopac.Job<'T>

module internal Job =

    module Global  =
        val start : J<'T> -> unit

    val start : J<'T> -> J<unit>
    val thunk : (unit -> 'T) -> J<'T>
    val unit : unit -> J<unit>
    val result : 'T -> J<'T>

    module Infixes =
        val ( >>= ) : J<'A> -> ('A -> J<'B>) -> J<'B>
        val ( >>. ) : J<'A> -> J<'B> -> J<'B>
        val ( .>> ) : J<'A> -> J<'B> -> J<'A>
        val ( |>> ) : J<'A> -> ('A -> 'B) -> J<'B>

type internal JobBuilder

type JobBuilder with
    new : unit -> JobBuilder
    member Bind : J<'A> * ('A -> J<'B>) -> J<'B>
    member Combine : J<unit> * J<'T> -> J<'T>
    member Delay : (unit -> J<'T>) -> J<'T>
    member For : seq<'T> * ('T -> J<unit>) -> J<unit>
    member For : array<'T> * ('T -> J<unit>) -> J<unit>
    member Return : 'T -> J<'T>
    member ReturnFrom : J<'T> -> J<'T>
    member While : (unit -> bool) * J<unit> -> J<unit>
    member Zero : unit -> J<unit>

module internal Alt =
    val always : 'T -> A<'T>
    val unit : unit -> A<unit>
    val never : unit -> A<'T>
    val zero : unit -> A<unit>
    val guard : J<A<'T>> -> A<'T>
    val delay : (unit -> A<'T>) -> A<'T>
    val withNack : (A<unit> -> J<A<'T>>) -> A<'T>
    val choose : seq<A<'T>> -> A<'T>
    module Infixes =
        val ( <|> ) : A<'T> -> A<'T> -> A<'T>
        val ( >>=? ) : A<'A> -> ('A -> J<'B>) -> A<'B>
        val ( >>.? ) : A<_> -> J<'T> -> A<'T>
        val ( .>>? ) : A<'T> -> J<_> -> A<'T>
        val ( |>>? ) : A<'A> -> ('A -> 'B) -> A<'B>
        val ( >>%? ) : A<'A> -> 'B -> A<'B>
    val pick : A<'T> -> J<'T>
    val select : seq<A<'T>> -> J<'T>

module internal Ch =
    module Now =
        val create : unit -> C<'T>
    module Alt =
        val give : C<'T> -> 'T -> A<unit>
        val take : C<'T> -> A<'T>
    module Global =
        val send : C<'T> -> 'T -> unit
    val create : unit -> J<C<'T>>
    val give : C<'T> -> 'T -> J<unit>
    val send : C<'T> -> 'T -> J<unit>
    val take : C<'T> -> J<'T>

[<AutoOpen>]
module internal TopLevel =
    val job : Hopac.JobBuilder
