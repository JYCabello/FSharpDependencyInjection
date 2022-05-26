[<Microsoft.FSharp.Core.RequireQualifiedAccess>]
module InstructionDefinitions

module User =
  open DSL
  open UserDsl

  let getUser id = UserProgram(GetUser(id, Pure))
  let getSettings userId = UserProgram(GetSettings(userId, Pure))
  let getDevice userId = UserProgram(GetDevice(userId, Pure))

module Email =
  open DSL
  open EmailDsl

  let send envelope = EmailProgram(Send(envelope, Pure))
