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

type Effect<'a> = IPorts -> Async<Result<'a, DomainError>>

let mapE (f: 'a -> 'b) (e: Effect<'a>) : Effect<'b> = fun p -> p |> e |> AsyncResult.map f

let bindE (f: 'a -> Effect<'b>) (e: Effect<'a>) : Effect<'b> =
  fun p ->
    asyncResult {
      let! a = e p
      return! p |> f a
    }

type EffectBuilder() =
  member this.Bind(x: Effect<'a>, f) : Effect<'b> = bindE f x
  member this.Return x : Effect<'a> = fun _ -> AsyncResult.ok x
  member this.ReturnFrom x = fun (_:IPorts) -> x
  member this.Zero() : Effect<Unit> = fun _ -> AsyncResult.ok ()

let effect = EffectBuilder()

let findUser id (p: IPorts) =
  match id with
  | 2 -> AsyncResult.error <| Unauthorized "user"
  | id -> p.runQuery "query" { ID = id; Name = "Name"; Email = "email@email.com" }

let findSettings id (p: IPorts) =
  match id with
  | 3 -> AsyncResult.error Conflict
  | userID -> p.runQuery "query" { UserID = userID; AreNotificationsEnabled = true }

let findDevice id (p: IPorts) =
  match id with
  | 4 -> AsyncResult.error <| NotFound "device"
  | 7 -> failwith "A weird happenstance"
  | userID -> p.runQuery "query" { UserID = userID; ID = userID + 7 }

let trySendDeviceID userID : Effect<_> =
  effect {
    let! user = findUser userID
    return user
  }

let program () : int -> Async<Result<Unit, DomainError>> =
  fun _ -> "not implmemented" |> InternalServerError |> AsyncResult.error

printfn "Hello from F#"
