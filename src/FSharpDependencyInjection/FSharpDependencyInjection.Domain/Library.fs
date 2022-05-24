namespace FSharpDependencyInjection.Domain

module DomainModel =
  type DomainError =
    | Unauthorized of protectedResourceName: string
    | NotFound of resourceName: string
    | Conflict

  type User =
    { Name: string
      ID: int
      Email: string }

  type UserSettings =
    { AreNotificationsEnabled: bool
      UserID: int }
    
  type Device =
    { UserID: int
      ID: int }

  let renderError =
    function
     | Unauthorized protectedResourceName ->
       $"Tried to access {protectedResourceName} but had no permissions"
     | Conflict ->
       "System in invalid state"
     | NotFound resourceName ->
       $"Could not find a resource of type {resourceName}"
