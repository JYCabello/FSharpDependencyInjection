module FSharpDependencyInjection.Effects

open FSharpDependencyInjection.Domain.DomainModel
open FsToolkit.ErrorHandling

type IPorts =
  abstract member runQuery<'a> : string -> 'a -> Async<Result<'a, DomainError>>
  abstract member sendEmail: EmailEnvelope -> Async<Result<Unit, DomainError>>

type Ports() =
  interface IPorts with
    member this.runQuery _ defaultValue = defaultValue |> AsyncResult.ok
    member this.sendEmail _ = () |> AsyncResult.ok

type Effect<'a> = IPorts -> Async<Result<'a, DomainError>>

let mapE (f: 'a -> 'b) (e: Effect<'a>) : Effect<'b> = fun p -> p |> e |> AsyncResult.map f

let bindE (f: 'a -> Effect<'b>) (e: Effect<'a>) : Effect<'b> =
  fun p ->
    asyncResult {
      let! a = e p
      return! p |> f a
    }

let getPorts (p: IPorts) = p |> AsyncResult.ok

type EffectBuilder() =
  member this.Bind(x: Effect<'a>, f: 'a -> Effect<'b>) : Effect<'b> = bindE f x
  member this.Return x : Effect<'a> = fun _ -> AsyncResult.ok x
  member this.ReturnFrom x = fun (_: IPorts) -> x
  member this.Zero() : Effect<Unit> = fun _ -> AsyncResult.ok ()
  member this.Combine(a, b) = a |> bindE (fun _ -> b)

let effect = EffectBuilder()
