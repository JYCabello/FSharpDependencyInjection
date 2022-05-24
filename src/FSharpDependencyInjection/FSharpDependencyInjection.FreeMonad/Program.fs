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

  type Error = string
  type Effect<'a> = Async<Result<'a, Error>>

  let ofResult r: Async<Result<'a, Error>> = r |> Async.singleton
  let singleton x: Async<Result<'a, Error>> = x |> Ok |> ofResult

  let bind (f: 'a -> Async<Result<'b, Error>>) (x: Async<Result<'a, Error>>): Async<Result<'b, Error>> = 
    asyncResult {
      let! value = x
      return! f value
    }

  let (>>=) x f = bind f x 
  
module UserInstructionsElevations =
  open ImpureInstructions
  let getUser id = Free (GetUser (id, Pure))
  let getSettings userId = Free (GetSettings (userId, Pure))
  let getDevice userId = Free (GetDevice (userId, Pure))

module Interpreters =
  open ImpureInstructions
  open Effects
  open FsToolkit.ErrorHandling
  let findUser id = singleton { ID = id; Name = "Name"; Email = "email@email.com" }
  let findSettings userId = { UserID = userId; AreNotificationsEnabled = true }
  let findDevice userId = { UserID = userId; ID = userId + 7 }
  
  let rec interpreter =
    function
    | Pure x -> singleton x
    | Free (GetUser (x, next)) -> findUser x |> AsyncResult.map next >>= interpreter
    | Free (GetSettings (x, next)) -> x |> findSettings |> next |> interpreter
    | Free (GetDevice (x, next)) -> x |> findDevice |> next |> interpreter

  
open ImpureInstructions
open UserInstructionsElevations
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

let result =
  program
  |> Interpreters.interpreter
  |> Async.RunSynchronously
  |> function
     | Ok value -> $"%A{value}"
     | Error errorValue -> errorValue

// For more information see https://aka.ms/fsharp-
printfn $"%A{result}"
