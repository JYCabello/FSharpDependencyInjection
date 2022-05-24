module Interpreters
open FSharpDependencyInjection.Domain
open FsToolkit.ErrorHandling
open DSL
open DomainModel

let (>>=) x f = AsyncResult.bind f x 
let rec build userInt emailInt =
  function
  | Pure p -> AsyncResult.ok p
  | UserProgram u -> u |> userInt >>= build userInt emailInt
  | EmailProgram e -> e |> emailInt >>= build userInt emailInt

module User =
  open UserDsl

  let findUser id =
    match id with
    | 2 -> AsyncResult.error <| Unauthorized "user"
    | _ -> AsyncResult.ok { ID = id; Name = "Name"; Email = "email@email.com" }
    
  let findSettings userID =
    match userID with
    | 3 -> Error Conflict
    | _ -> Ok { UserID = userID; AreNotificationsEnabled = true }
    
  let findDevice userID =
    match userID with
    | 4 -> Error <| NotFound "device"
    | 7 -> failwith "A weird happenstance"
    | _ -> Ok { UserID = userID; ID = userID + 7 }

  let interpreter =
    function
    | GetUser (x, next) -> findUser x |> AsyncResult.map next
    | GetSettings (x, next) -> x |> findSettings |> Result.map next |> Async.singleton
    | GetDevice (x, next) -> x |> findDevice |> Result.map next |> Async.singleton

module Email =
  open EmailDsl
  let sendEmail (_: EmailEnvelope) = () |> AsyncResult.ok
  
  let interpreter =
    function
    | Send (envelope, next) -> sendEmail envelope |> AsyncResult.map next
