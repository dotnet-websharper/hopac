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

open System
open System.Collections.Generic
open IntelliFactory.WebSharper

type A<'T> = Hopac.Alt<'T>
type C<'T> = Hopac.Ch<'T>
type J<'T> = Hopac.Job<'T>

[<AbstractClass>]
[<JavaScript>]
[<Proxy(typeof<Hopac.Job<_>>)>]
type Job<'T> () =
    abstract Run : ('T -> unit) -> unit

[<JavaScript>]
[<Sealed>]
type PrimJob<'T> (run) =
    inherit Job<'T> ()
    override jb.Run k = run k

[<JavaScript>]
[<Sealed>]
type BindJob<'A,'B> (x: Job<'A>, f: 'A -> Job<'B>) =
    inherit Job<'B> ()

    override j.Run k =
        x.Run (fun x ->
            Executor.spawn (fun () ->
                let y = f x
                Executor.spawn (fun () ->
                    y.Run k)))

[<JavaScript>]
[<Sealed>]
type ResultJob<'T> (x: 'T) =
    inherit Job<'T> ()
    override j.Run k = k x

[<AutoOpen>]
module JobNotation =

    [<Inline "$0">]
    [<M(MO.NoInlining)>]
    let ( !+ ) (j: Job<'T>) : J<'T> = U

    [<Inline "$0">]
    [<M(MO.NoInlining)>]
    let ( !- ) (j: J<'T>) : Job<'T> = U

[<JavaScript>]
[<Proxy "Hopac.Job, Hopac">]
module Job =

    let result x = !+ (ResultJob x)
    let unit () = result ()

    [<Proxy "Hopac.Job.Infixes, Hopac">]
    module Infixes =

        let ( >>= ) x (f: _ -> J<_>) =
            !+ (BindJob (!-x, fun x -> !- (f x)))

        let ( >>. ) a b = a >>= fun _ -> b
        let ( .>> ) a b = a >>= fun x -> b >>= fun _ -> result x
        let ( |>> ) x f = x >>= fun x -> result (f x)
        let ( >>% ) x y = x >>= fun _ -> result y

    open Infixes

    let thunk f = result () |>> f

    let delay (f: unit -> J<'T>) =
        !+ PrimJob(fun kont -> (!- f()).Run kont)

    [<Proxy "Hopac.Job.Global, Hopac">]
    module Global =

        let start job =
            Executor.spawn (fun () -> (!-job).Run ignore)

    let start job =
        thunk (fun () -> Global.start job)

open Job.Infixes

[<JavaScript>]
[<Proxy(typeof<Hopac.JobBuilder>)>]
[<Sealed>]
type JobBuilder () =

    member jb.Return (v: 'T) = Job.result v
    member jb.ReturnFrom (j: J<'T>) = j
    member jb.Bind (a, f) = a >>= f
    member jb.Combine (a: J<unit>, b: J<'T>) = a >>. b
    member jb.Delay f = Job.delay f

    member jb.While (cond: unit -> bool, job: J<unit>) : J<unit> =
        !+ PrimJob(fun k ->
            let rec loop () =
                if cond () then
                    Executor.spawn (fun () -> (!- job).Run loop)
                else
                    Executor.spawn k
            loop ())

    member jb.For (xs: seq<'T>, f: 'T -> J<unit>) =
        jb {
            let e = xs.GetEnumerator ()
            while e.MoveNext () do
                do! f e.Current
        }

    member jb.For (xs: 'T[], f) =
        jb.For(xs :> seq<_>, f)

    member jb.Zero () =
        Job.result ()

[<AutoOpen>]
[<JavaScript>]
module JobNotation2 =
    let job = JobBuilder ()

type Transaction<'T> =
    {
        Cont : 'T -> J<unit>
        IsActive : ref<bool>
        Resources : Queue<IDisposable>
        Select : int -> unit
    }

type Exit<'T> =
    {
        Ix : int
        Tr : Transaction<'T>
    }

[<JavaScript>]
module Transaction =

    [<M(MO.NoInlining)>]
    let create k set =
        {
            Cont = k
            IsActive = ref true
            Resources = Queue ()
            Select = set
        }

    [<M(MO.NoInlining)>]
    let attach tr res =
        tr.Resources.Enqueue res

    [<M(MO.NoInlining)>]
    let mapJob f tr =
        let k = tr.Cont
        {
            Cont = fun a ->
                job {
                    let! b = f a
                    return! k b
                }
            IsActive = tr.IsActive
            Resources = tr.Resources
            Select = tr.Select
        }

[<JavaScript>]
module Exit =

    [<M(MO.NoInlining)>]
    let create i t =
        { Ix = i; Tr = t }

    let throw { Ix = index; Tr = tr } v =
        tr.IsActive := false
        tr.Resources.ToArray()
        |> Array.iter (fun r -> r.Dispose ())
        tr.Select index
        Job.Global.start (tr.Cont v)

    [<M(MO.NoInlining)>]
    let mapJob f e =
        { Ix = e.Ix; Tr = Transaction.mapJob f e.Tr }

type Branch<'T> =
    {
        Poll : unit -> bool
        Suspend : Exit<'T> -> unit
        Sync : ('T -> unit) -> unit
    }

[<JavaScript>]
module Branch =

    let mapJob f b =
        {
            Poll = b.Poll
            Suspend = fun e -> b.Suspend (Exit.mapJob f e)
            Sync = fun k -> b.Sync (fun v -> (!-f(v)).Run(k))
        }

[<JavaScript>]
[<Sealed>]
type ChanQueue<'T> (isActive: 'T -> bool) =
    let mutable qu = Queue<'T> ()

    member q.IsEmpty = qu.Count = 0
    member q.Enqueue v = qu.Enqueue v
    member q.Dequeue () = qu.Dequeue ()
    member q.ToArray () = qu.ToArray ()

    /// Drops all inactive items.
    member q.Clear () =
        let items = qu.ToArray ()
        if Array.exists (isActive >> not) items then
            let r = Queue<'T> ()
            qu.ToArray ()
            |> Array.iter (fun item ->
                if isActive item then
                    r.Enqueue item)
            qu <- r

    interface IDisposable with
        member q.Dispose () = q.Clear ()

[<AbstractClass>]
[<JavaScript>]
[<Proxy(typeof<Hopac.Alt<_>>)>]
type Alt<'T> () =
    inherit Job<'T> ()

    abstract Init : selected: A<int> -> pos: int -> J<Branch<'T>[]>

    override alt.Run cont =
        let job =
            job {
                let selected = IVar<int>()
                let! events = alt.Init (As<A<int>> selected) 0
                return (
                    let picked =
                        events
                        |> Array.tryFindIndex (fun e -> e.Poll ())
                    match picked with
                    | None ->
                        let tr =
                            selected.Fill
                            |> Transaction.create (fun v -> job { return cont v })
                        events
                        |> Array.iteri (fun i ev ->
                            ev.Suspend (Exit.create i tr))
                    | Some i ->
                        selected.Fill i
                        events.[i].Sync cont
                )
            }
        (!-job).Run ignore

and [<JavaScript>]
    [<Sealed>]
    [<Proxy(typeof<Hopac.IVar<_>>)>]
    IVar<'T> () =
    inherit Alt<'T> ()

    let mutable isReady = false
    let mutable state = U
    let mutable conts = new ChanQueue<Exit<'T>> (fun ex -> !ex.Tr.IsActive)

    let self =
        [|{
            Poll = fun () -> isReady
            Suspend = fun ex ->
                conts.Enqueue ex
                Transaction.attach ex.Tr conts
            Sync = fun k -> k state
        }|]

    override alt.Init _ _ =
        Job.result self

    member var.Fill (v: 'T) : unit =
        if isReady then
            failwith "IVar: set twice"
        isReady <- true
        state <- v
        conts.ToArray ()
        |> Array.iter (fun e -> Exit.throw e v)
        conts <- U

[<AutoOpen>]
module AltNotation =

    [<Inline "$0">]
    [<M(MO.NoInlining)>]
    let ( !+? ) (x: Alt<'T>) : A<'T> = U

    [<Inline "$0">]
    [<M(MO.NoInlining)>]
    let ( !-? ) (x: A<'T>) : Alt<'T> = U

[<JavaScript>]
[<Sealed>]
type AlwaysAlt<'T> (v: 'T) =
    inherit Alt<'T> ()

    let self =
        [|{
            Poll = fun _ -> true
            Suspend = ignore
            Sync = fun k -> k v
        }|]

    override alt.Init _ _ =
        Job.result self

[<JavaScript>]
[<Sealed>]
type NeverAlt<'T> () =
    inherit Alt<'T> ()
    let self = Array.empty

    override alt.Init _ _ =
        Job.result Array.empty

[<JavaScript>]
[<Sealed>]
type ChoiceAlt<'T> (a: Alt<'T>, b: Alt<'T>) =
    inherit Alt<'T> ()

    override alt.Init st pos =
        job {
            let! xs = a.Init st pos
            let! ys = b.Init st (pos + xs.Length)
            return Array.append xs ys
        }

[<JavaScript>]
[<Sealed>]
type ChooseAlt<'T> (xs: A<'T>[]) =
    inherit Alt<'T> ()

    override alt.Init st pos =
        job {
            let out = ResizeArray ()
            let pos = ref pos
            for x in xs do
                let! br = (!-? x).Init st !pos
                do out.AddRange br
                do pos := !pos + br.Length
            return out.ToArray ()
        }

[<JavaScript>]
[<Sealed>]
type GuardAlt<'T> (guard: J<A<'T>>) =
    inherit Alt<'T> ()

    override alt.Init st pos =
        job {
            let! alt = guard
            return! (!-? alt).Init st pos
        }

[<JavaScript>]
[<Sealed>]
type WrapAlt<'A,'B> (a: Alt<'A>, f: 'A -> J<'B>) =
    inherit Alt<'B> ()

    override alt.Init st pos =
        job {
            let! br = a.Init st pos
            return Array.map (Branch.mapJob f) br
        }

[<JavaScript>]
module Nacks =

    let createAlt (choice: A<int>) (pos: int) =
        let res = IVar ()
        job {
            let! c = choice
            if c <> pos then
                return res.Fill ()
        }
        |> Job.Global.start
        !+? res

[<JavaScript>]
[<Sealed>]
type WithNackAlt<'T> (f: A<unit> -> J<A<'T>>) =
    inherit Alt<'T> ()

    override alt.Init choice pos =
        job {
            let nack = Nacks.createAlt choice pos
            let! x = f nack
            return! (!-? x).Init choice pos
        }

[<JavaScript>]
[<Proxy "Hopac.Alt, Hopac">]
module Alt =
    let always v = !+? (AlwaysAlt v)
    let never () = !+? (NeverAlt ())
    let pick (alt: A<'T>) : J<'T> = As alt
    let unit () = always ()
    let zero () = never () : A<unit>

    let choose xs = !+? ChooseAlt(Seq.toArray xs)
    let select xs = pick (choose xs)
    let guard g = !+? (GuardAlt g)
    let delay f = guard (Job.thunk f)
    let withNack f = !+? (WithNackAlt f)

    [<Proxy "Hopac.Alt.Infixes, Hopac">]
    module Infixes =
        let ( <|> ) a b = !+? ChoiceAlt(!-? a, !-? b)
        let ( >>=? ) x f = !+? WrapAlt(!-? x, f)
        let ( >>.? ) a b = a >>=? fun _ -> b
        let ( .>>? ) x y = x >>=? fun x -> y >>% x
        let ( |>>? ) x f = x >>=? fun x -> Job.result (f x)
        let ( >>%? ) x v = x >>=? fun _ -> Job.result v

// Channels -------------------------------------------------------------------

type Chan<'T> =
    {
        Readers : ChanQueue<Exit<'T>>
        Writers : ChanQueue<'T * Exit<unit>>
    }

[<JavaScript>]
module Chan =

    let send ch v =
        ch.Readers.Clear ()
        if ch.Readers.IsEmpty then
            let t = Transaction.create (fun _ -> Job.result ()) ignore
            let e = Exit.create 0 t
            ch.Writers.Enqueue (v, e)
        else
            let e = ch.Readers.Dequeue ()
            Exit.throw e v

    let create () =
        {
            Readers = new ChanQueue<Exit<'T>>(fun e -> e.Tr.IsActive.Value)
            Writers = new ChanQueue<'T * Exit<unit>>(fun (_, e) -> e.Tr.IsActive.Value)
        }

[<JavaScript>]
[<Proxy(typeof<Hopac.Ch<_>>)>]
type Ch<'T> (st: Chan<'T>) =
    inherit Alt<'T> ()

    let take =
        [|{
            Poll = fun () ->
                st.Writers.Clear ()
                not st.Writers.IsEmpty
            Suspend = fun ex ->
                st.Readers.Enqueue ex
                Transaction.attach ex.Tr st.Readers
            Sync = fun cont ->
                let (value, e) = st.Writers.Dequeue ()
                Exit.throw e ()
                cont value
        }|]

    member ch.Chan = st

    override alt.Init _ _ =
        Job.result take

[<JavaScript>]
[<Sealed>]
type GiveAlt<'T> (ch: Chan<'T>, value: 'T) =
    inherit Alt<unit> ()

    let give =
        [|{
            Poll = fun () ->
                ch.Readers.Clear ()
                not ch.Readers.IsEmpty
            Suspend = fun ex ->
                ch.Writers.Enqueue (value, ex)
                Transaction.attach ex.Tr ch.Writers
            Sync = fun cont ->
                let e = ch.Readers.Dequeue ()
                Exit.throw e value
                cont ()
        }|]

    override alt.Init _ _ =
        Job.result give

[<Proxy "Hopac.Ch, Hopac">]
module Ch =
    module A = Alt

    let ( !++ ) (a: Ch<'T>) = As<C<'T>> a
    let ( !-- ) (a: C<'T>) = As<Ch<'T>> a
    let (|Ch|) (ch: C<'T>) = (!--ch).Chan

    [<Proxy "Hopac.Ch.Now, Hopac">]
    module Now =
        let create () = !++ Ch(Chan.create ())

    [<Proxy "Hopac.Ch.Global, Hopac">]
    module Global =
        let send (Ch chan) v = Chan.send chan v

    let create () =
        job { return Now.create () }

    [<Proxy "Hopac.Ch.Alt, Hopac">]
    module Alt =
        let give (Ch chan) value = !+? GiveAlt(chan, value)
        let take (ch: C<'T>) = As<A<'T>> ch

    let give ch v = A.pick (Alt.give ch v)
    let take (ch: C<'T>) = A.pick ch
    let send ch v = job { return Global.send ch v }

[<AutoOpen>]
[<JavaScript>]
[<Proxy "Hopac.TopLevel, Hopac">]
module TopLevel =
    let job = As<Hopac.JobBuilder> (JobBuilder ())
