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
    
  let sendEmail (_: EmailEnvelope) = () |> AsyncResult.ok

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
    trySendEmail findUser (findSettings >> Async.singleton) (findDevice >> Async.singleton) sendEmail

[1..10]
|> List.map (execute CompositionalRoot.trySendEmailComposed)
|> List.map (printfn "%s")
|> ignore
