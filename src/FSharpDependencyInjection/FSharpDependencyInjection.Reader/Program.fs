module FSharpDependencyInjection.Reader

open FSharpDependencyInjection.Domain.DomainModel
open FsToolkit.ErrorHandling

module Program =
  open ReaderEdge

  let findUser id (p: IPorts) =
    match id with
    | 2 -> AsyncResult.error <| Unauthorized "user"
    | id ->
      p.runQuery
        "query"
        { ID = id
          Name = "Name"
          Email = "email@email.com" }

  let findSettings id (p: IPorts) =
    match id with
    | 3 -> AsyncResult.error Conflict
    | userID ->
      p.runQuery
        "query"
        { UserID = userID
          AreNotificationsEnabled = true }

  let findDevice id (p: IPorts) =
    match id with
    | 4 -> AsyncResult.error <| NotFound "device"
    | 7 -> failwith "A weird happenstance"
    | userID -> p.runQuery "query" { UserID = userID; ID = userID + 7 }

  let private getProgram getUser getSettings getDevice () =
    let trySendDeviceID userID : Effect<_> =
      effect {
        let! user = getUser userID
        let! settings = getSettings userID
        let! device = getDevice userID
        let! p = askE

        return!
          match settings.AreNotificationsEnabled with
          | false -> () |> AsyncResult.ok
          | true ->
            p.sendEmail
              { To = user.Email
                Subject = "Hi"
                Body = $"Your device ID is {device.ID}" }
      }

    trySendDeviceID

  let program () : int -> Async<Result<Unit, DomainError>> =
    fun userID -> getProgram findUser findSettings findDevice () userID (Ports())
