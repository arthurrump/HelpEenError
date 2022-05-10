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

module Afsluiting =
    type FormConfig =
        { OpgelostVanLogboek: FieldConfig<int, int>
          OpgelostInVergelijking: FieldConfig<int, int>
          TijdBesteed: FieldConfig<int, int> }

    type Form =
        { OpgelostVanLogboek: Field<int>
          OpgelostInVergelijking: Field<int>
          TijdBesteed: Field<int> }

    type Result =
        { OpgelostVanLogboek: int
          OpgelostInVergelijking: int
          TijdBesteed: int }

    [<RequireQualifiedAccess>]
    module Validate =
        let form (c: FormConfig) (m: Form) =
            validate {
                let! ol = Field.validate c.OpgelostVanLogboek m.OpgelostVanLogboek
                and! ov = Field.validate c.OpgelostInVergelijking m.OpgelostInVergelijking
                and! tb = Field.validate c.TijdBesteed m.TijdBesteed
                return
                    { OpgelostVanLogboek = ol
                      OpgelostInVergelijking = ov
                      TijdBesteed = tb }
            }
        let result (m: Result) =
            validate {
                let! ol = Validators.NL.likert "Aandeel opgeloste fouten in het logboek" m.OpgelostVanLogboek
                and! ov = Validators.NL.likert "Hoeveelheid opgeloste fouten in vergelijking" m.OpgelostInVergelijking
                and! tb = Validators.NL.likert "Hoeveelheid tijd besteed aan fouten" m.TijdBesteed
                return
                    { OpgelostVanLogboek = ol
                      OpgelostInVergelijking = ov
                      TijdBesteed = tb }
            }

    [<RequireQualifiedAccess>]
    module Form =
        let config () : FormConfig =
            { OpgelostVanLogboek =
                Field.config ("Aandeel opgeloste fouten in het logboek", Validators.NL.required Validators.NL.likert)
              OpgelostInVergelijking =
                Field.config ("Hoeveelheid opgeloste fouten in vergelijking", Validators.NL.required Validators.NL.likert)
              TijdBesteed =
                Field.config ("Hoeveelheid tijd besteed aan fouten", Validators.NL.required Validators.NL.likert) }

        let init () : Form =
            { OpgelostVanLogboek = Field.init ()
              OpgelostInVergelijking = Field.init ()
              TijdBesteed = Field.init () }

        let validateAll (config: FormConfig) (form: Form) =
            { form with
                OpgelostVanLogboek = Field.update config.OpgelostVanLogboek Field.Validate form.OpgelostVanLogboek
                OpgelostInVergelijking = Field.update config.OpgelostInVergelijking Field.Validate form.OpgelostInVergelijking
                TijdBesteed = Field.update config.TijdBesteed Field.Validate form.TijdBesteed }
