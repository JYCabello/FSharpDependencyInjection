namespace FSharpDependencyInjection.Domain

module DomainModel =
  type DomainError =
    | Unauthorized of protectedResourceName: string
    | NotFound of resourceName: string
    | Conflict
    | InternalServerError of errorMessage: string

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

  type EmailEnvelope =
    { To: string
      Subject: string
      Body: string }
