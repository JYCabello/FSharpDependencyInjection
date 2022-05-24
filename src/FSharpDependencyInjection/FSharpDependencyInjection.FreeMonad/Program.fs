open FSharpDependencyInjection.Domain
open FSharpDependencyInjection.Domain.DomainModel
open DSL
open InstructionDefinitions.Email
open InstructionDefinitions.User
open ErrorHandling

type FinalResult =
  { DeviceID: int
    ShouldSendEmail: bool
    Email: string }

let trySendDeviceViaEmail userID =
  dsl {
    let! user = getUser userID
    let! settings = getSettings userID
    let! device = getDevice userID

    return!
      match settings.AreNotificationsEnabled with
      | false -> Pure ()
      | true -> send { To = user.Email; Subject = "Hi"; Body = $"Your device ID is {device.ID}" }
  }

let execute program userID =
  program userID
  |> Interpreters.build Interpreters.User.interpreter Interpreters.Email.interpreter
  |> Async.RunSynchronously
  |> function
      | Ok _ -> $"Successfully completed with id {userID}"
      | Error error -> renderError error

[1..5]
|> List.map (execute trySendDeviceViaEmail)
|> List.map (printfn "%s")
|> ignore
