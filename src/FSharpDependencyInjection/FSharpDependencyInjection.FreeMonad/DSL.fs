module DSL
open UserDsl
open EmailDsl

type Program<'a> =
| Pure of 'a
| UserProgram of UserInstructions<Program<'a>>
| EmailProgram of EmailInstructions<Program<'a>>

let rec bind f =
  function
  | Pure p -> f p
  | UserProgram up -> up |> mapUser (bind f) |> UserProgram
  | EmailProgram ep -> ep |> mapEmail (bind f) |> EmailProgram

type DSLBuilder () =
  member this.Bind (x, f) = bind f x
  member this.Return x = Pure x
  member this.ReturnFrom x = x
  member this.Zero () = Pure ()

let dsl = DSLBuilder()
