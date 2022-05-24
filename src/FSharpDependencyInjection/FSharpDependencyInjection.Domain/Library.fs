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
