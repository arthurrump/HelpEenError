namespace Shared.Models

open Validus
open Shared.Form
open Shared.Library

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

module Demografisch =
    type FormConfig =
        { Schoolniveau: FieldConfig<string, string>
          ProgrammeerVaardigheid: FieldConfig<int, int>
          ProgrammeerErvaring: FieldConfig<int, int> }

    type Form =
        { Schoolniveau: Field<string>
          ProgrammeerVaardigheid: Field<int>
          ProgrammeerErvaring: Field<int> }

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
        let form (c: FormConfig) (m: Form) =
            validate {
                let! sn = Field.validate c.Schoolniveau m.Schoolniveau
                and! pv = Field.validate c.ProgrammeerVaardigheid m.ProgrammeerVaardigheid
                and! pe = Field.validate c.ProgrammeerErvaring m.ProgrammeerErvaring
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
        let config () : FormConfig =
            { Schoolniveau =
                Field.config ("Schoolniveau", Validators.NL.required Validate.schoolniveau)
              ProgrammeerVaardigheid =
                Field.config ("Je inschatting van programmeervaardigheden", Validators.NL.required Validators.NL.likert)
              ProgrammeerErvaring =
                Field.config ("Je programmeerervaring", Validators.NL.required Validators.NL.likert) }

        let init () : Form =
            { Schoolniveau = Field.init ()
              ProgrammeerVaardigheid = Field.init ()
              ProgrammeerErvaring = Field.init () }

        let validateAll (config: FormConfig) (form: Form) =
            { form with
                Schoolniveau = Field.update config.Schoolniveau Field.Validate form.Schoolniveau
                ProgrammeerVaardigheid = Field.update config.ProgrammeerVaardigheid Field.Validate form.ProgrammeerVaardigheid
                ProgrammeerErvaring = Field.update config.ProgrammeerErvaring Field.Validate form.ProgrammeerErvaring }

module Interview =
    type FormConfig =
        { Meedoen: FieldConfig<string, bool>
          Naam: FieldConfig<string, string>
          Klas: FieldConfig<string, string>
          School: FieldConfig<string, string>
          Email: FieldConfig<string, string>
          OuderDan16: FieldConfig<string, bool>
          EmailOuders: FieldConfig<string, string> }

    type Form =
        { Meedoen: Field<string>
          Naam: Field<string>
          Klas: Field<string>
          School: Field<string>
          Email: Field<string>
          OuderDan16: Field<string>
          EmailOuders: Field<string> }

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
        let form (c: FormConfig) (m: Form) =
            validate {
                let! meedoen = Field.validate c.Meedoen m.Meedoen
                if meedoen then
                    let! naam = Field.validate c.Naam m.Naam
                    and! klas = Field.validate c.Klas m.Klas
                    and! school = Field.validate c.School m.School
                    and! email = Field.validate c.Email m.Email
                    and! ouderDan16 = Field.validate c.OuderDan16 m.OuderDan16
                    let! toestemming =
                        if ouderDan16 then
                            validate {
                                return Toestemming.Alleen
                            }
                        else
                            validate {
                                let! emailOuders = Field.validate c.EmailOuders m.EmailOuders
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
                    let! email = Validate.email "Email Ouders" emailOuders
                    return Toestemming.Ouders email
            }
        let contact (m: Contact) =
            validate {
                let! naam = Validate.nietLeeg "Naam" m.Naam
                and! klas = Validate.nietLeeg "Klas" m.Klas
                and! school = Validate.nietLeeg "School" m.School
                and! email = Validate.email "Email" m.Email
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
        let config () : FormConfig =
            { Meedoen =
                Field.config ("Meedoen", Validators.NL.required Validate.jaNee)
              Naam =
                Field.config ("Naam", Validators.NL.required Validate.nietLeeg)
              Klas =
                Field.config ("Klas", Validators.NL.required Validate.nietLeeg)
              School =
                Field.config ("School", Validators.NL.required Validate.nietLeeg)
              Email =
                Field.config ("Email", Validators.NL.required Validate.email)
              OuderDan16 =
                Field.config ("Ouder dan 16", Validators.NL.required Validate.jaNee)
              EmailOuders =
                Field.config ("Email Ouders", Validators.NL.required Validate.email) }

        let init () : Form =
            { Meedoen = Field.init ()
              Naam = Field.init ()
              Klas = Field.init ()
              School = Field.init ()
              Email = Field.init ()
              OuderDan16 = Field.init ()
              EmailOuders = Field.init () }

        let validateAll (config: FormConfig) (form: Form) =
            { form with
                Meedoen = Field.update config.Meedoen Field.Validate form.Meedoen
                Naam = Field.update config.Naam Field.Validate form.Naam
                Klas = Field.update config.Klas Field.Validate form.Klas
                School = Field.update config.School Field.Validate form.School
                Email = Field.update config.Email Field.Validate form.Email
                OuderDan16 = Field.update config.OuderDan16 Field.Validate form.OuderDan16
                EmailOuders = Field.update config.EmailOuders Field.Validate form.EmailOuders }

module Logboek =
    type FormConfig =
        { Type: FieldConfig<string, string> // Foutmelding, OnverwachtGedrag
          FoutmeldingMelding: FieldConfig<string, string>
          OnverwachtGedragBeschrijving: FieldConfig<string, string>
          OnverwachtGedragVerwachting: FieldConfig<string, string>
          Vervolgactie: FieldConfig<string, string> }

    type Form =
        { Type: Field<string> // Foutmelding, OnverwachtGedrag
          FoutmeldingMelding: Field<string>
          OnverwachtGedragBeschrijving: Field<string>
          OnverwachtGedragVerwachting: Field<string>
          Vervolgactie: Field<string> }

    type Result =
        | Foutmelding of melding: string * vervolgactie: string
        | OnverwachtGedrag of beschrijving: string * verwachting: string * vervolgactie: string

    [<RequireQualifiedAccess>]
    module Validate =
        let type' =
            Validator.create
                (sprintf "%s moet 'foutmelding' of 'onverwacht-gedrag' zijn.")
                (fun sn -> sn = "foutmelding" || sn = "onverwacht-gedrag")
        let form (c: FormConfig) (m: Form) =
            validate {
                let! t = Field.validate c.Type m.Type
                if t = "foutmelding" then
                    let! melding = Field.validate c.FoutmeldingMelding m.FoutmeldingMelding
                    let! vervolgactie = Field.validate c.Vervolgactie m.Vervolgactie
                    return Foutmelding (melding, vervolgactie)
                else
                    let! beschrijving = Field.validate c.OnverwachtGedragBeschrijving m.OnverwachtGedragBeschrijving
                    let! verwachting = Field.validate c.OnverwachtGedragVerwachting m.OnverwachtGedragVerwachting
                    let! vervolgactie = Field.validate c.Vervolgactie m.Vervolgactie
                    return OnverwachtGedrag (beschrijving, verwachting, vervolgactie)
            }
        let result (m: Result) =
            match m with
            | Foutmelding (melding, vervolgactie) ->
                validate {
                    let! melding = Validate.nietLeeg "Foutmelding" melding
                    let! vervolgactie = Validate.nietLeeg "Vervolgactie" vervolgactie
                    return Foutmelding (melding, vervolgactie)
                }
            | OnverwachtGedrag (beschrijving, verwachting, vervolgactie) ->
                validate {
                    let! beschrijving = Validate.nietLeeg "Beschrijving" beschrijving
                    let! verwachting = Validate.nietLeeg "Verwachting" verwachting
                    let! vervolgactie = Validate.nietLeeg "Vervolgactie" vervolgactie
                    return OnverwachtGedrag (beschrijving, verwachting, vervolgactie)
                }

    module Form =
        let config () : FormConfig =
            { Type =
                Field.config ("Type", Validators.NL.required Validate.type')
              FoutmeldingMelding =
                Field.config ("Foutmelding", Validators.NL.required Validate.nietLeeg)
              OnverwachtGedragBeschrijving =
                Field.config ("Beschrijving", Validators.NL.required Validate.nietLeeg)
              OnverwachtGedragVerwachting =
                Field.config ("Verwachting", Validators.NL.required Validate.nietLeeg)
              Vervolgactie =
                Field.config ("Vervolgactie", Validators.NL.required Validate.nietLeeg) }

        let init () : Form =
            { Type = Field.init ()
              FoutmeldingMelding = Field.init ()
              OnverwachtGedragBeschrijving = Field.init ()
              OnverwachtGedragVerwachting = Field.init ()
              Vervolgactie = Field.init () }

        let validateAll (config: FormConfig) (form: Form) =
            { form with
                Type = Field.update config.Type Field.Validate form.Type
                FoutmeldingMelding = Field.update config.FoutmeldingMelding Field.Validate form.FoutmeldingMelding
                OnverwachtGedragBeschrijving = Field.update config.OnverwachtGedragBeschrijving Field.Validate form.OnverwachtGedragBeschrijving
                OnverwachtGedragVerwachting = Field.update config.OnverwachtGedragVerwachting Field.Validate form.OnverwachtGedragVerwachting
                Vervolgactie = Field.update config.Vervolgactie Field.Validate form.Vervolgactie }
