module FSharpDependencyInjection.Domain.Endpoint
open ErrorHandling

let execute program userID =
  program userID
  |> attempt
  |> Async.RunSynchronously
  |> function
      | Ok _ -> $"All good with id {userID}"
      | Error error -> renderError error

let runOneToTen program =
  [1..10]
  |> List.map (execute program)
  |> List.map (printfn "%s")
  |> ignore
