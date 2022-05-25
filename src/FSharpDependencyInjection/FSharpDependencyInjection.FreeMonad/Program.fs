module FSharpDependencyInjection.FreeMonad
open FSharpDependencyInjection.Domain
open FSharpDependencyInjection.Domain.DomainModel
open DSL
open InstructionDefinitions.Email
open InstructionDefinitions.User

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

let buildInterpreter () =
  Interpreters.build Interpreters.User.interpreter Interpreters.Email.interpreter

let program id =
  trySendDeviceViaEmail id
  |> buildInterpreter ()

Endpoint.runOneToTen program
