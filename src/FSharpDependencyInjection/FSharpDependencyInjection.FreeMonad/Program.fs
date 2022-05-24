open FSharpDependencyInjection.Domain.DomainModel

module FreeProgram =
  open UserDomain
  open EmailDomain

  type Program<'a> =
  | Pure of 'a
  | UserProgram of UserInstructions<Program<'a>>
  | EmailProgram of EmailInstructions<Program<'a>>
  
  let rec bind f =
    function
    | Pure p -> f p
    | UserProgram up -> up |> mapUser (bind f) |> UserProgram
    | EmailProgram ep -> ep |> mapEmail (bind f) |> EmailProgram
    
  type DomainBuilder () =
    member this.Bind (x, f) = bind f x
    member this.Return x = Pure x
    member this.ReturnFrom x = x
    member this.Zero () = Pure ()
    
  let domainLanguage = DomainBuilder()
  
module UserInstructionsDefinitions =
  open FreeProgram
  open UserDomain
  let getUser id = UserProgram (GetUser (id, Pure))
  let getSettings userId = UserProgram (GetSettings (userId, Pure))
  let getDevice userId = UserProgram (GetDevice (userId, Pure))

module EmailInstructionsDefinitions =
  open FreeProgram
  open EmailDomain
  let send envelope = EmailProgram (Send (envelope, Pure))
  
open FreeProgram
open UserInstructionsDefinitions
open EmailInstructionsDefinitions

type FinalResult =
  { DeviceID: int
    ShouldSendEmail: bool
    Email: string }

let program userID =
  domainLanguage {
    let! user = getUser userID
    let! settings = getSettings userID
    let! device = getDevice userID

    let emailOperation =
      match settings.AreNotificationsEnabled with
      | false -> Pure ()
      | true -> send { To = user.Email; Subject = "Hi"; Body = $"Your device ID is {device.ID}" }
      
    return! emailOperation
  }

module Interpreters =
  open FsToolkit.ErrorHandling
  module User =
    open UserDomain
    
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
      | _ -> Ok { UserID = userID; ID = userID + 7 }

    let interpreter =
      function
      | GetUser (x, next) -> findUser x |> AsyncResult.map next
      | GetSettings (x, next) -> x |> findSettings |> Result.map next |> Async.singleton
      | GetDevice (x, next) -> x |> findDevice |> Result.map next |> Async.singleton

  module Email =
    open EmailDomain
    let sendEmail (_: EmailEnvelope) = () |> AsyncResult.ok
    
    let interpreter =
      function
      | Send (envelope, next) -> sendEmail envelope |> AsyncResult.map next

  let (>>=) x f = AsyncResult.bind f x 
  let rec build userInt emailInt =
    let recursion = build userInt emailInt

    function
    | Pure p -> AsyncResult.ok p
    | UserProgram u -> u |> userInt >>= recursion
    | EmailProgram e -> e |> emailInt >>= recursion

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
