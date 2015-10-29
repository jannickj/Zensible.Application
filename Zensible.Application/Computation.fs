namespace Zensible.Application

// Type for side effect free computations which can fail
type Computation<'result,'error> = Success of 'result
                                 | Failure of 'error


module Computation =
    
        let fromOption error = function
            | Some res -> Success res
            | None -> Failure error
            
        let map f = function
            | Success x -> Success (f x)
            | Failure e -> Failure e
        
        type ComputationBinder() =
            member this.Return(x : 'result) = Success x
            member this.ReturnFrom(x : Computation<'result,'error>) = x
            member this.Fail (x: 'error) = Failure x
            member this.Bind(x, f) = 
                match x with
                    | Success r -> f r
                    | Failure l -> Failure l
    
        let computation = ComputationBinder()

        let fmap : ('a -> 'b) -> Computation<'a,_> -> Computation<'b,_> =
          fun f ma ->
            computation {
                let! a = ma
                return f a
            }
    
        let (<!>) = fmap

        // asynchronious call
        let (<*>) : Computation<'a -> 'b, 'error> -> Computation<'a, 'error> -> Computation<'b, 'error> =
          fun fa ma ->
            computation {
                let! f = fa
                let! a = ma

                return f a
            }

        let (<*) (fa : Computation<'a, 'e>) (fb : Computation<'a, 'e>) : Computation<'a, 'e> =
            computation {
                let! a = fa
                let! _ = fb

                return a
            }

        let (>>=) x f =
            computation.Bind(x, f)

        let (=<<) f x =
            computation.Bind(x, f)