module FSharpDependencyInjection.Domain.ErrorHandling
open DomainModel
open FsToolkit.ErrorHandling


let renderError =
  function
  | Unauthorized protectedResourceName ->
    $"Tried to access {protectedResourceName} but had no permissions"
  | Conflict ->
    "System in invalid state"
  | NotFound resourceName ->
    $"Could not find a resource of type {resourceName}"
  | InternalServerError message ->
    $"All hell broke loose: {message}"

let attempt (ar: Async<Result<'a, DomainError>>): Async<Result<'a, DomainError>> =
  asyncResult {
    try
      return! ar
    with
    | ex -> return! InternalServerError ex.Message |> Error
  }
