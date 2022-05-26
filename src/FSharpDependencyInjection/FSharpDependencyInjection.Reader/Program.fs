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

type Effect<'a, 'b> = IPorts -> Async<Result<'a, 'b>>

let mapE (f: 'a -> 'b) (e: Effect<'a, 'e>) : Effect<'b, 'e> = fun p -> p |> e |> AsyncResult.map f

let bindE (f: 'a -> Async<Result<'b, 'e>>) (e: Effect<'a, 'e>) : Effect<'b, 'e> =
  fun p -> p |> e |> AsyncResult.bind f

type GetUser = int -> Effect<User, DomainError>
type GetSettings = int -> Effect<UserSettings, DomainError>
type GetDevice = int -> Effect<Device, DomainError>
type SendEmail = EmailEnvelope -> Effect<Unit, DomainError>


let program () : int -> Async<Result<Unit, DomainError>> =
  fun _ -> "not implmemented" |> InternalServerError |> AsyncResult.error

printfn "Hello from F#"
