module FSharpDependencyInjection.Reader

open FSharpDependencyInjection.Domain.DomainModel
open FsToolkit.ErrorHandling

type IPorts =
  abstract member runQuery<'a> : string -> 'a -> Async<Result<'a, DomainError>>
  abstract member sendEmail: EmailEnvelope -> Async<Result<Unit, DomainError>>

type Ports =
  interface IPorts with
    member this.runQuery _ defaultValue = defaultValue |> AsyncResult.ok
    member this.sendEmail _ = () |> AsyncResult.ok
  
    

let program () : int -> Async<Result<Unit, DomainError>> =
  fun _ -> "not implmemented" |> InternalServerError |> AsyncResult.error

printfn "Hello from F#"
