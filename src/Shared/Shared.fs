namespace Shared.Models

open System
open Validus
open Shared.Form
open Shared.Library

module Demografisch =
    type Form =
        { Schoolniveau: Field<string, string>
          ProgrammeerVaardigheid: Field<int, int>
          ProgrammeerErvaring: Field<int, int> }

    type Result =
        { Schoolniveau: string
          ProgrammeerVaardigheid: int
          ProgrammeerErvaring: int }

    [<RequireQualifiedAccess>]
    module Validate =
        let schoolniveau =
            Validator.create
                (sprintf "%s moet 'havo' of 'vwo' zijn.")
                (fun sn -> sn = "havo" || sn = "vwo")
        let form (m: Form) =
            validate {
                let! sn = Field.validate m.Schoolniveau
                and! pv = Field.validate m.ProgrammeerVaardigheid
                and! pe = Field.validate m.ProgrammeerErvaring
                return
                    { Schoolniveau = sn
                      ProgrammeerVaardigheid = pv
                      ProgrammeerErvaring = pe }
            }
        let result (m: Result) =
            validate {
                let! sn = schoolniveau "Schoolniveau" m.Schoolniveau
                and! pv = Validators.NL.likert "Programmeervaardigheid" m.ProgrammeerVaardigheid
                and! pe = Validators.NL.likert "Programmeerervaring" m.ProgrammeerErvaring
                return
                    { Schoolniveau = sn
                      ProgrammeerVaardigheid = pv
                      ProgrammeerErvaring = pe }
            }

    [<RequireQualifiedAccess>]
    module Form =
        let init () : Form =
            { Schoolniveau =
                Field.init ("Schoolniveau", Validators.NL.required Validate.schoolniveau)
              ProgrammeerVaardigheid =
                Field.init ("Je inschatting van programmeervaardigheden", Validators.NL.required Validators.NL.likert)
              ProgrammeerErvaring =
                Field.init ("Je programmeerervaring", Validators.NL.required Validators.NL.likert) }

        let validateAll (form: Form) =
            { form with
                Schoolniveau = Field.update Field.Validate form.Schoolniveau
                ProgrammeerVaardigheid = Field.update Field.Validate form.ProgrammeerVaardigheid
                ProgrammeerErvaring = Field.update Field.Validate form.ProgrammeerErvaring }


type Todo = { Id: Guid; Description: string }

module Todo =
    let isValid (description: string) =
        String.IsNullOrWhiteSpace description |> not

    let create (description: string) =
        { Id = Guid.NewGuid()
          Description = description }

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type ITodosApi =
    { getTodos: unit -> Async<Todo list>
      addTodo: Todo -> Async<Todo> }
