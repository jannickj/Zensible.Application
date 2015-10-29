namespace Zensible.Application

// Type for computations with side effects which can fail.
type App<'result,'error> = Async<Computation<'result,'error>>

module App =
    open Computation

    type AppBinder() =
        member this.Return(x : 'T) = async { return (Success x) }
        member this.ReturnFrom(x : Async<'T>) = x
        member this.Fail (x: 'T) = async { return Failure x }
        member this.Bind(x, f) = async {
            let! result = x
            match result with
                | Success r -> return! (f r)
                | Failure l -> return (Failure l)
        }

    let app = AppBinder()

    let reply comp : App<'result, 'error> = async { return comp }
    let replySuccess = app.Return
    let replyFailure (error : 'error) = Failure error

    let fmap : ('a -> 'b) -> App<'a,_> -> App<'b,_> =
      fun f ma ->
        app {
            let! a = ma
            return f a
        }
    
    let (<!>) = fmap

    // asynchronious call
    let (<*>) : App<'a -> 'b, 'error> -> App<'a, 'error> -> App<'b, 'error> =
      fun fa ma ->
         app {
            ignore ma
            let! f = fa
            let! a = ma

            return f a
        }

    let (<*) (fa : App<'a, 'e>) (fb : App<'a, 'e>) : App<'a, 'e> =
        app {
            ignore fb
            let! a = fa
            let! _ = fb

            return a
        }

    let (>>=) x f =
        app.Bind(x, f)

    let (=<<) f x =
        app.Bind(x, f)

    let fromAsyncOption (error: 'b) ( ma : Async<Option<'a>> ) : App<'a, 'b> = 
        async {
            let! res = ma
            return fromOption error res
        } 

    let fromAsync (errorFun : exn -> 'error) (ma : Async<'result>) : App<'result,'error> =
        async {
            let! a = Async.Catch ma
            match a with
            | Choice1Of2 res -> return Success res
            | Choice2Of2 exn -> return Failure (errorFun exn)
        }