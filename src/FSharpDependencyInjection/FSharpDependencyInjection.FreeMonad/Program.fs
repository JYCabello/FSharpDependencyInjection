open FSharpDependencyInjection.Domain.DomainModel
open DSL
open InstructionDefinitions.Email
open InstructionDefinitions.User

type FinalResult =
  { DeviceID: int
    ShouldSendEmail: bool
    Email: string }

let program userID =
  dsl {
    let! user = getUser userID
    let! settings = getSettings userID
    let! device = getDevice userID

    let emailOperation =
      match settings.AreNotificationsEnabled with
      | false -> Pure ()
      | true -> send { To = user.Email; Subject = "Hi"; Body = $"Your device ID is {device.ID}" }
      
    return! emailOperation
  }

let result userID =
  program userID
  |> Interpreters.build Interpreters.User.interpreter Interpreters.Email.interpreter
  |> Async.RunSynchronously
  |> function
      | Ok _ -> $"Successfully completed with id {userID}"
      | Error error -> renderError error

[1..5]
|> List.map result
|> List.map (printfn "%s")
|> ignore
