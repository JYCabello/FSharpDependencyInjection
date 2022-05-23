open FSharpDependencyInjection.Domain.DomainModel

module ImpureInstructions =
  type UserInstructions<'a> =  
  | GetUser of (int * (User -> 'a))
  | GetSettings of (int * (UserSettings -> 'a))
  | GetDevice of (int * (Device -> 'a))
  
  let private mapI f =
    function
    | GetUser (x, next) -> GetUser (x, next >> f)
    | GetSettings (x, next) -> GetSettings (x, next >> f)
    | GetDevice (x, next) -> GetDevice (x, next >> f)

  type UserProgram<'a> =
  | Free of UserInstructions<UserProgram<'a>>
  | Pure of 'a
  
  let rec bind f =
    function
    | Free x -> x |> mapI (bind f) |> Free
    | Pure x -> f x
  
// For more information see https://aka.ms/fsharp-
printfn "Hello from F#"
