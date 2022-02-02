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

module Interview =
    type Form =
        { Meedoen: Field<string, bool>
          Naam: Field<string, string>
          Klas: Field<string, string>
          School: Field<string, string>
          Email: Field<string, string>
          OuderDan16: Field<string, bool>
          EmailOuders: Field<string, string> }

    [<RequireQualifiedAccess>]
    type Toestemming =
        | Alleen
        | Ouders of email: string

    type Contact =
        { Naam: string
          Klas: string
          School: string
          Email: string
          Toestemming: Toestemming }

    [<RequireQualifiedAccess>]
    type Result =
        | Ja of Contact
        | Nee

    [<RequireQualifiedAccess>]
    module Validate =
        open Validus.Operators
        let jaNee = fun field value ->
            if value = "ja" then Ok true
            else if value = "nee" then Ok false
            else Error (ValidationErrors.create field [ $"%s{field} moet 'ja' of 'nee' zijn." ])
        let email =
            Validators.String.betweenLen 8 512
                (sprintf "%s moet tussen de 8 en 512 tekens lang zijn.")
            <+> Validators.String.contains "@"
                (sprintf "%s moet een geldig emailadres zijn.")
        let nietLeeg =
            Validators.String.notEmpty (sprintf "%s mag niet leeg zijn.")
        let form (m: Form) =
            validate {
                let! meedoen = Field.validate m.Meedoen
                if meedoen then
                    let! naam = Field.validate m.Naam
                    and! klas = Field.validate m.Klas
                    and! school = Field.validate m.School
                    and! email = Field.validate m.Email
                    and! ouderDan16 = Field.validate m.OuderDan16
                    let! toestemming =
                        if ouderDan16 then
                            validate {
                                return Toestemming.Alleen
                            }
                        else
                            validate {
                                let! emailOuders = Field.validate m.EmailOuders
                                return Toestemming.Ouders emailOuders
                            }
                    return Result.Ja
                        { Naam = naam
                          Klas = klas
                          School = school
                          Email = email
                          Toestemming = toestemming }
                else
                    return Result.Nee
            }
        let toestemming (m: Toestemming) =
            validate {
                match m with
                | Toestemming.Alleen ->
                    return Toestemming.Alleen
                | Toestemming.Ouders emailOuders ->
                    let! email = email "Email Ouders" emailOuders
                    return Toestemming.Ouders email
            }
        let contact (m: Contact) =
            validate {
                let! naam = nietLeeg "Naam" m.Naam
                and! klas = nietLeeg "Klas" m.Klas
                and! school = nietLeeg "School" m.School
                and! email = email "Email" m.Email
                and! toestemming = toestemming m.Toestemming
                return
                    { Naam = naam
                      Klas = klas
                      School = school
                      Email = email
                      Toestemming = toestemming }
            }
        let result (m: Result) =
            validate {
                match m with
                | Result.Ja c ->
                    let! c = contact c
                    return Result.Ja c
                | Result.Nee ->
                    return Result.Nee
            }

    module Form =
        let init () : Form =
            { Meedoen =
                Field.init ("Meedoen", Validators.NL.required Validate.jaNee)
              Naam =
                Field.init ("Naam", Validators.NL.required Validate.nietLeeg)
              Klas =
                Field.init ("Klas", Validators.NL.required Validate.nietLeeg)
              School =
                Field.init ("School", Validators.NL.required Validate.nietLeeg)
              Email =
                Field.init ("Email", Validators.NL.required Validate.email)
              OuderDan16 =
                Field.init ("Ouder dan 16", Validators.NL.required Validate.jaNee)
              EmailOuders =
                Field.init ("Email Ouders", Validators.NL.required Validate.email) }

        let validateAll (form: Form) =
            { form with
                Meedoen = Field.update Field.Validate form.Meedoen
                Naam = Field.update Field.Validate form.Naam
                Klas = Field.update Field.Validate form.Klas
                School = Field.update Field.Validate form.School
                Email = Field.update Field.Validate form.Email
                OuderDan16 = Field.update Field.Validate form.OuderDan16
                EmailOuders = Field.update Field.Validate form.EmailOuders }

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
