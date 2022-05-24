module FSharpDependencyInjection.Domain.ErrorHandling
open DomainModel
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
