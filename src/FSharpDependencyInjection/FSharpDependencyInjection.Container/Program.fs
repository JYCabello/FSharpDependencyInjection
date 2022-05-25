open FSharpDependencyInjection.Domain
open FSharpDependencyInjection.Domain.DomainModel
open FsToolkit.ErrorHandling
open Microsoft.Extensions.DependencyInjection

type UserRepository () =
  member _.GetUser: int -> Async<Result<User, DomainError>> =
    function
    | 2 -> AsyncResult.error <| Unauthorized "user"
    | id -> AsyncResult.ok { ID = id; Name = "Name"; Email = "email@email.com" }
    
  member _.GetSettings: int -> Result<UserSettings,DomainError> =
    function
    | 3 -> Error Conflict
    | userID -> Ok { UserID = userID; AreNotificationsEnabled = true }
  
  member _.GetDevice: int -> Result<Device ,DomainError> =
    function
    | 4 -> Error <| NotFound "device"
    | 7 -> failwith "A weird happenstance"
    | userID -> Ok { UserID = userID; ID = userID + 7 }

type EmailClient () =
  member _.Send: EmailEnvelope -> unit = fun _ -> ()

type NotificationService(userRepository: UserRepository, emailClient: EmailClient) =
  member this.TrySendEmail userID : Async<Result<unit, DomainError>> =
    asyncResult {
      let! user = userRepository.GetUser userID
      let! settings = userRepository.GetSettings userID
      let! device = userRepository.GetDevice userID

      return
        match settings.AreNotificationsEnabled with
        | false -> ()
        | true -> emailClient.Send { To = user.Email; Subject = "Hi"; Body = $"Your device ID is {device.ID}" }
    }

open ErrorHandling

let provider =
  ServiceCollection()
    .AddSingleton<UserRepository>()
    .AddSingleton<EmailClient>()
    .AddSingleton<NotificationService>()
    .BuildServiceProvider()

let execute program userID =
  program userID
  |> attempt
  |> Async.RunSynchronously
  |> function
      | Ok _ -> $"All good with id {userID}"
      | Error error -> renderError error

let service = provider.GetService<NotificationService>()

[1..10]
|> List.map (execute service.TrySendEmail)
|> List.map (printfn "%s")
|> ignore
