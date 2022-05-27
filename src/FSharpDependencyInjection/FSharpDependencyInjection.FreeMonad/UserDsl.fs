module UserDsl

open FSharpDependencyInjection.Domain.DomainModel

type UserInstructions<'a> =
  | GetUser of (int * (User -> 'a))
  | GetSettings of (int * (UserSettings -> 'a))
  | GetDevice of (int * (Device -> 'a))

let mapUser f =
  function
  | GetUser (id, next) -> GetUser(id, next >> f)
  | GetSettings (userID, next) -> GetSettings(userID, next >> f)
  | GetDevice (userID, next) -> GetDevice(userID, next >> f)
