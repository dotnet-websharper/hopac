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

[<AbstractClass>]
type Job<'T> () =
    abstract Run : ('T -> unit) -> unit

[<Sealed>]
type PrimJob<'T> (run) =
    inherit Job<'T> ()
    override jb.Run k = run k

[<Sealed>]
type BindJob<'A,'B> (x: Job<'A>, f: 'A -> Job<'B>) =
    inherit Job<'B> ()

    override j.Run k =
        x.Run (fun x ->
            Executor.spawn (fun () ->
                let y = f x
                Executor.spawn (fun () ->
                    y.Run k)))

[<Sealed>]
type ResultJob<'T> (x: 'T) =
    inherit Job<'T> ()
    override j.Run k = k x

module Job =

    let result x = ResultJob x :> Job<_>
    let unit () = result ()

    module Infixes =
        let ( >>= ) x f = BindJob (x, f) :> Job<_>
        let ( >>. ) a b = a >>= fun _ -> b
        let ( .>> ) a b = a >>= fun x -> b >>= fun _ -> result x
        let ( |>> ) x f = x >>= fun x -> result (f x)
        let ( >>% ) x y = x >>= fun _ -> result y

    open Infixes

    let thunk f = result () |>> f

    let delay (f: unit -> Job<'T>) =
        PrimJob (fun kont -> f().Run(kont)) :> Job<_>

    module Global =

        let start (job: Job<_>) =
            Executor.spawn (fun () -> job.Run ignore)

open Job.Infixes

[<Sealed>]
type JobBuilder () =

    member jb.Return (v: 'T) = Job.result v
    member jb.ReturnFrom (j: Job<'T>) = j
    member jb.Bind (a, f) = a >>= f
    member jb.Combine (a: Job<unit>, b: Job<'T>) = a >>. b
    member jb.Delay f = Job.delay f

    member jb.While (cond: unit -> bool, job: Job<unit>) =
        PrimJob (fun k ->
            let rec loop () =
                if cond () then
                    Executor.spawn (fun () -> job.Run loop)
                else
                    Executor.spawn k
            loop ()) :> Job<_>

    member jb.For (xs: seq<'T>, f: 'T -> Job<unit>) =
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
module JobNotation =
    let job = JobBuilder ()

type Transaction<'T> =
    {
        Cont : 'T -> Job<unit>
        IsActive : ref<bool>
        Resources : Queue<IDisposable>
        Select : int -> unit
    }

type Exit<'T> =
    {
        Ix : int
        Tr : Transaction<'T>
    }

module Transaction =

    let create k set =
        {
            Cont = k
            IsActive = ref true
            Resources = Queue ()
            Select = set
        }

    let attach tr res =
        tr.Resources.Enqueue res

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

module Exit =

    let create i t =
        { Ix = i; Tr = t }

    let throw { Ix = index; Tr = tr } v =
        tr.IsActive := false
        tr.Resources.ToArray()
        |> Array.iter (fun r -> r.Dispose ())
        tr.Select index
        Job.Global.start (tr.Cont v)

    let mapJob f e =
        { Ix = e.Ix; Tr = Transaction.mapJob f e.Tr }

type Branch<'T> =
    {
        Poll : unit -> bool
        Suspend : Exit<'T> -> unit
        Sync : ('T -> unit) -> unit
    }

module Branch =

    let mapJob f b =
        {
            Poll = b.Poll
            Suspend = fun e -> b.Suspend (Exit.mapJob f e)
            Sync = fun k -> b.Sync (fun v -> f(v).Run(k))
        }

[<AbstractClass>]
type Alt<'T> () =
    inherit Job<'T> ()

    abstract Init : selected: Alt<int> -> pos: int -> Job<Branch<'T>[]>

    override alt.Run cont =
        let job =
            job {
                let selected = IVar<int>()
                let! events = alt.Init selected 0
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
        job.Run ignore

and [<Sealed>] IVar<'T> () =
    inherit Alt<'T> ()

    let mutable isReady = false
    let mutable state = U
    let mutable conts = ResizeArray<Exit<'T>> ()

    let self =
        [|{
            Poll = fun () -> isReady
            Suspend = conts.Add
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

[<Sealed>]
type NeverAlt<'T> () =
    inherit Alt<'T> ()
    let self = Array.empty

    override alt.Init _ _ =
        Job.result Array.empty

[<Sealed>]
type ChoiceAlt<'T> (a: Alt<'T>, b: Alt<'T>) =
    inherit Alt<'T> ()

    override alt.Init st pos =
        job {
            let! xs = a.Init st pos
            let! ys = b.Init st (pos + xs.Length)
            return Array.append xs ys
        }

[<Sealed>]
type ChooseAlt<'T> (xs: Alt<'T>[]) =
    inherit Alt<'T> ()

    override alt.Init st pos =
        job {
            let out = ResizeArray ()
            let pos = ref pos
            for x in xs do
                let! br = x.Init st !pos
                do out.AddRange br
                do pos := !pos + br.Length
            return out.ToArray ()
        }

[<Sealed>]
type GuardAlt<'T> (guard: Job<Alt<'T>>) =
    inherit Alt<'T> ()

    override alt.Init st pos =
        job {
            let! alt = guard
            return! alt.Init st pos
        }

[<Sealed>]
type WrapAlt<'A,'B> (a: Alt<'A>, f: 'A -> Job<'B>) =
    inherit Alt<'B> ()

    override alt.Init st pos =
        job {
            let! br = a.Init st pos
            return Array.map (Branch.mapJob f) br
        }

module Nacks =

    let createAlt (choice: Alt<int>) (pos: int) =
        let res = IVar ()
        job {
            let! c = choice
            if c <> pos then
                return res.Fill ()
        }
        |> Job.Global.start
        res :> Alt<_>

[<Sealed>]
type WithNackAlt<'T> (f: Alt<unit> -> Job<Alt<'T>>) =
    inherit Alt<'T> ()

    override alt.Init choice pos =
        job {
            let nack = Nacks.createAlt choice pos
            let! x = f nack
            return! x.Init choice pos
        }

module Alt =
    let always v = AlwaysAlt v :> Alt<_>
    let never () = NeverAlt () :> Alt<_>
    let pick (alt: Alt<'T>) = alt :> Job<'T>
    let unit () = always ()
    let zero () = never () : Alt<unit>
    let choose xs = ChooseAlt (Seq.toArray xs) :> Alt<_>
    let select xs = pick (choose xs)
    let guard g = GuardAlt g :> Alt<_>
    let delay f = guard (Job.thunk f)
    let withNack f = WithNackAlt f :> Alt<_>

    module Infixes =
        let ( <|> ) a b = ChoiceAlt (a, b) :> Alt<_>
        let ( >>=? ) x f = WrapAlt (x, f) :> Alt<_>
        let ( >>.? ) a b = a >>=? fun _ -> b
        let ( .>>? ) x y = x >>=? fun x -> y >>% x
        let ( |>>? ) x f = x >>=? fun x -> Job.result (f x)
        let ( >>%? ) x v = x >>=? fun _ -> Job.result v

/// Channels ---------------------------------------------------------------

[<Sealed>]
type ChanQueue<'T> (isActive: 'T -> bool) =
    let mutable qu = Queue<'T> ()

    member q.IsEmpty = qu.Count = 0
    member q.Enqueue (v: 'T) = qu.Enqueue v
    member q.Dequeue () = qu.Dequeue ()
    member q.ToArray () = qu.ToArray ()

    /// Drops all inactive items.
    member q.Clear () =
        let r = Queue<'T> ()
        qu.ToArray ()
        |> Array.iter (fun item ->
            if isActive item then
                r.Enqueue item)
        qu <- r

    interface IDisposable with
        member q.Dispose () = q.Clear ()

type Chan<'T> =
    {
        Readers : ChanQueue<Exit<'T>>
        Writers : ChanQueue<'T * Exit<unit>>
    }

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

module Ch =
    module A = Alt

    let (|Ch|) (ch: Ch<'T>) = ch.Chan

    module Now =
        let create () = Ch (Chan.create ())

    module Global =
        let send (Ch chan) v = Chan.send chan v

    let create () =
        job { return Now.create () }

    module Alt =
        let give (Ch chan) value = GiveAlt (chan, value) :> Alt<_>
        let take (ch: Ch<'T>) = ch :> Alt<'T>

    let give ch v = A.pick (Alt.give ch v)
    let take (ch: Ch<'T>) = A.pick ch
    let send ch v = job { return Global.send ch v }

[<AutoOpen>]
module TopLevel =

    let job = JobBuilder ()
