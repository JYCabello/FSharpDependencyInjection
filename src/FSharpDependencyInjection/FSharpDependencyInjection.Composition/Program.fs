open FSharpDependencyInjection.Domain.DomainModel
open FsToolkit.ErrorHandling
open FSharpDependencyInjection.Domain.ErrorHandling

module Operations =
  type GetUser = int -> Async<Result<User, DomainError>>
  type GetSettings = int -> Async<Result<UserSettings, DomainError>>
  type GetDevice = int -> Async<Result<Device, DomainError>>
  type SendEmail = EmailEnvelope -> Async<Result<Unit, DomainError>>

open Operations
let trySendEmail
  (getUser: GetUser)
  (getSettings: GetSettings)
  (getDevice: GetDevice)
  (sendEmail: SendEmail)
  userId =
    asyncResult {
      let! user = getUser userId
      let! settings = getSettings userId
      let! device = getDevice userId
      
      return!
        match settings.AreNotificationsEnabled with
        | false -> () |> AsyncResult.ok
        | true -> sendEmail { To = user.Email; Subject = "Hi"; Body = $"Your device ID is {device.ID}" }
    }

module Implementations =
  let findUser =
    function
    | 2 -> AsyncResult.error <| Unauthorized "user"
    | id -> AsyncResult.ok { ID = id; Name = "Name"; Email = "email@email.com" }
    
  let findSettings =
    function
    | 3 -> Error Conflict
    | userID -> Ok { UserID = userID; AreNotificationsEnabled = true }
    
  let findDevice =
    function
    | 4 -> Error <| NotFound "device"
    | 7 -> failwith "A weird happenstance"
    | userID -> Ok { UserID = userID; ID = userID + 7 }
    
  let sendEmail (_: EmailEnvelope) = ()

let execute program userID =
  program userID
  |> attempt
  |> Async.RunSynchronously
  |> function
      | Ok _ -> $"All good with id {userID}"
      | Error error -> renderError error

module CompositionalRoot =
  open Implementations
  let trySendEmailComposed =
    trySendEmail findUser (findSettings >> Async.singleton) (findDevice >> Async.singleton) (sendEmail >> AsyncResult.ok)

[1..10]
|> List.map (execute CompositionalRoot.trySendEmailComposed)
|> List.map (printfn "%s")
|> ignore
