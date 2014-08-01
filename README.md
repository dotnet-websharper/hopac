WebSharper.Hopac
================

[WebSharper](http://websharper.com) bindings to
[Hopac](https://github.com/VesaKarvonen/Hopac) - expressive concurrency
in JavaScript.

WebSharper cross-compiles F# code to run in the browser via JavaScript, and Hopac implements
expressive concurrency combinators in F# inspired by Concurrent ML.  The aim of this project
is to be a bridge between the two.  By referencing `WebSharper.Hopac` in your project, you
should be able to use Hopac vocabulary on the *client*, and even share some code concurrency
patterns between client and server.

## Status

For the first version, we are aiming to cover some basics, such as Job combinators,
`Alt.choose`, `Alt.withNack`.. The very first iteration ignores exception semantics for
the moment.  Focus is on reasonably matching the semantics, although there are obvious
limitations of the WebSharper/JS enironment. For example, scheduling is *fully*
cooperative, uses a single CPU only; basically context switches cannot interrupt a
basic `a -> b` function, though they can be inserted automatically inside `Job<'T>`.

## License

See https://github.com/intellifactory/websharper/blob/master/LICENSE.md

