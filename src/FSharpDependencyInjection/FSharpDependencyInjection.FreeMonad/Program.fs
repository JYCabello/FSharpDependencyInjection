﻿open FSharpDependencyInjection.Domain.DomainModel

module UserDomain =
  type UserInstructions<'a> =  
  | GetUser of (int * (User -> 'a))
  | GetSettings of (int * (UserSettings -> 'a))
  | GetDevice of (int * (Device -> 'a))
  
  let mapUser f =
    function
    | GetUser (x, next) -> GetUser (x, next >> f)
    | GetSettings (x, next) -> GetSettings (x, next >> f)
    | GetDevice (x, next) -> GetDevice (x, next >> f)

module EmailDomain =
  type EmailInstructions<'a> =
  | SendEmail of (EmailEnvelope * (Unit -> 'a))
  
  let mapEmail f =
    function
    | SendEmail(emailEnvelope, next) -> SendEmail(emailEnvelope, next >> f)

module FreeProgram =
  open UserDomain

  type Program<'a> =
  | UserProgram of UserInstructions<Program<'a>>
  | Pure of 'a
  
  let rec bind f =
    function
    | UserProgram x -> x |> mapUser (bind f) |> UserProgram
    | Pure x -> f x
    
  type UserBuilder () =
    member this.Bind (x, f) = bind f x
    member this.Return x = Pure x
    member this.ReturnFrom x = x
    member this.Zero () = Pure ()
    
  let user = UserBuilder()
  
module UserInstructionsDefinitions =
  open FreeProgram
  open UserDomain
  let getUser id = UserProgram (GetUser (id, Pure))
  let getSettings userId = UserProgram (GetSettings (userId, Pure))
  let getDevice userId = UserProgram (GetDevice (userId, Pure))

  
open FreeProgram
open UserInstructionsDefinitions

type FinalResult =
  { DeviceID: int
    ShouldSendEmail: bool
    Email: string }

let program =
  user {
    let! user = getUser 5
    let! settings = getSettings user.ID
    let! device = getDevice user.ID

    return
      { DeviceID = device.ID
        ShouldSendEmail = settings.AreNotificationsEnabled
        Email = user.Email }
  }

module Interpreters =
  open FsToolkit.ErrorHandling
  module User =
    open UserDomain
    let findUser id = async { return Ok { ID = id; Name = "Name"; Email = "email@email.com" } }
    let findSettings userId = Ok { UserID = userId; AreNotificationsEnabled = true }
    let findDevice userId = { UserID = userId; ID = userId + 7 }
    
    let interpreter =
      function
      | GetUser (x, next) -> findUser x |> AsyncResult.map next
      | GetSettings (x, next) -> x |> findSettings |> Result.map next |> Async.singleton
      | GetDevice (x, next) -> x |> findDevice |> next |> AsyncResult.ok
  
  let (>>=) x f = AsyncResult.bind f x 
  let rec build userInterpreter =
    function
    | Pure p -> AsyncResult.ok p
    | UserProgram f -> f |> userInterpreter >>= build userInterpreter

let result =
  program
  |> Interpreters.build Interpreters.User.interpreter
  |> Async.RunSynchronously
  |> function
      | Ok value -> $"%A{value}"
      | Error error -> renderError error

// For more information see https://aka.ms/fsharp-
printfn $"%A{result}"
