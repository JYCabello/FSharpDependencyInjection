module Tests

open System
open Xunit
open FSharpDependencyInjection

let data: Object array array =
  [| (1, "All good with id 1")
     (2, "Tried to access user but had no permissions")
     (3, "System in invalid state")
     (4, "Could not find a resource of type device")
     (5, "All good with id 5")
     (6, "All good with id 6")
     (7, "All hell broke loose: A weird happenstance")
     (8, "All good with id 8")
     (9, "All good with id 9")
     (10, "All good with id 10") |]
  |> Array.collect (fun (userID, result) ->
    [| Container.program ()
       Composition.CompositionalRoot.trySendEmailComposed ()
       FreeMonad.program
       Reader.Program.program () |]
    |> Array.map (fun program ->
      [| userID :> Object
         result :> Object
         program :> Object |]))

open FSharpDependencyInjection.Domain.Endpoint

[<Theory; MemberData("data")>]
let ``Every way of dependency injection provides the same output`` userID expectation program =
  Assert.Equal(expectation, execute program userID)
