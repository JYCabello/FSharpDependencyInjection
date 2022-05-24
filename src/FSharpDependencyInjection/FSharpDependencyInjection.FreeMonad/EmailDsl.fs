module EmailDsl

open FSharpDependencyInjection.Domain.DomainModel

type EmailInstructions<'a> =
| Send of (EmailEnvelope * (Unit -> 'a))

let mapEmail f =
  function
  | Send (emailEnvelope, next) -> Send(emailEnvelope, next >> f)
