open FSharpDependencyInjection.Domain.DomainModel

module ImpureInstructions =
  type UserInstructions<'a> =  
  | GetUser of (int * (User -> 'a))
  | GetSettings of (int * (UserSettings -> 'a))
  | GetDevice of (int * (Device -> 'a))
  
  let private mapI f =
    function
    | GetUser (x, next) -> GetUser (x, next >> f)
    | GetSettings (x, next) -> GetSettings (x, next >> f)
    | GetDevice (x, next) -> GetDevice (x, next >> f)

  type UserProgram<'a> =
  | Free of UserInstructions<UserProgram<'a>>
  | Pure of 'a
  
  let rec bind f =
    function
    | Free x -> x |> mapI (bind f) |> Free
    | Pure x -> f x
    
  type UserBuilder () =
    member this.Bind (x, f) = bind f x
    member this.Return x = Pure x
    member this.ReturnFrom x = x
    member this.Zero () = Pure ()
    
  let user = UserBuilder()
  
module Effects =
  open FsToolkit.ErrorHandling

  let ofResult r: Async<Result<'a, 'e>> = r |> Async.singleton
  let singleton x: Async<Result<'a, 'e>> = x |> Ok |> ofResult

  let bind (f: 'a -> Async<Result<'b, 'e>>) (x: Async<Result<'a, 'e>>): Async<Result<'b, 'e>> = 
    asyncResult {
      let! value = x
      return! f value
    }

  let (>>=) x f = bind f x 
  
module UserInstructionsDefinitions =
  open ImpureInstructions
  let getUser id = Free (GetUser (id, Pure))
  let getSettings userId = Free (GetSettings (userId, Pure))
  let getDevice userId = Free (GetDevice (userId, Pure))

  
open ImpureInstructions
open UserInstructionsDefinitions
open FsToolkit.ErrorHandling

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
  open Effects
  let findUser id = async { return Ok { ID = id; Name = "Name"; Email = "email@email.com" } }
  let findSettings userId = async { return Ok { UserID = userId; AreNotificationsEnabled = true } }
  let findDevice userId = { UserID = userId; ID = userId + 7 }
  
  let rec interpreter userProgram =
    match userProgram with
    | Pure x -> singleton x
    | Free (GetUser (x, next)) -> findUser x |> AsyncResult.map next >>= interpreter
    | Free (GetSettings (x, next)) -> x |> findSettings |> AsyncResult.map next >>= interpreter
    | Free (GetDevice (x, next)) -> x |> findDevice |> next |> interpreter

let result =
  program
  |> Interpreters.interpreter
  |> Async.RunSynchronously
  |> function
     | Ok value -> $"%A{value}"
     | Error errorValue ->
       match errorValue with
       | Unauthorized protectedResourceName -> $"Tried to access {protectedResourceName} but had no permissions"
       | Conflict -> "System in invalid state"
       | NotFound resourceName -> $"Could not find a resource of type {resourceName}"

// For more information see https://aka.ms/fsharp-
printfn $"%A{result}"
