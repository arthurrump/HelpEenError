module InterviewForm

[<RequireQualifiedAccess>]
type Toestemming =
    | Alleen
    | Ouders of email: string

type InterviewContact =
    { Naam: string
      Klas: string
      School: string
      Email: string
      Toestemming: Toestemming }

[<RequireQualifiedAccess>]
type InterviewDeelname =
    | Ja of InterviewContact
    | Nee

type Fields =
    { Meedoen: string
      Naam: string
      Klas: string
      School: string
      Email: string
      OuderDan16: string
      EmailOuders: string }

module Fields =
    let submittable fields =
        fields.Meedoen = "nee" ||
        fields.Meedoen = "ja" &&
        fields.Naam <> "" &&
        fields.Klas <> "" &&
        fields.School <> "" &&
        fields.Email <> "" &&
        fields.OuderDan16 <> "" &&
        (fields.OuderDan16 = "ja" || fields.EmailOuders <> "")

    let toResult fields =
        if fields.Meedoen = "ja" then
            InterviewDeelname.Ja
                { Naam = fields.Naam
                  Klas = fields.Klas
                  School = fields.School
                  Email = fields.Email
                  Toestemming =
                    if fields.OuderDan16 = "nee"
                    then Toestemming.Ouders fields.EmailOuders
                    else Toestemming.Alleen }
        else
            InterviewDeelname.Nee

type Msg =
    | MeedoenChanged of string
    | NaamChanged of string
    | KlasChanged of string
    | SchoolChanged of string
    | EmailChanged of string
    | OuderDan16Changed of string
    | EmailOudersChanged of string

let init () =
    { Meedoen = ""
      Naam = ""
      Klas = ""
      School = ""
      Email = ""
      OuderDan16 = ""
      EmailOuders = "" }

let update msg (model: Fields) =
    match msg with
    | MeedoenChanged meedoen ->
        { model with Meedoen = meedoen }
    | NaamChanged naam ->
        { model with Naam = naam }
    | KlasChanged klas ->
        { model with Klas = klas }
    | SchoolChanged school ->
        { model with School = school }
    | EmailChanged email ->
        { model with Email = email }
    | OuderDan16Changed jongerDan16 ->
        { model with OuderDan16 = jongerDan16 }
    | EmailOudersChanged emailOuders ->
        { model with EmailOuders = emailOuders }

open Feliz
open Feliz.Bulma
open FormHelpers

let view model onChange onSubmit =
    Html.form [
        prop.onSubmit (fun ev ->
            ev.preventDefault ()
            onSubmit (Fields.toResult model)
        )
        prop.children [
            Html.p [
                spacing.mb5
                prop.text ("Met het invullen van dit logboek help je ons al heel veel, maar we gaan ook graag met een aantal "
                    + "van jullie in gesprek over de fouten en foutmeldingen die jullie vinden tijdens het programmeren. "
                    + "Zo'n interview duurt ongeveer 15 minuten en doe je samen met 1 of 2 klasgenoten.")
            ]
            Form.radio(
                title = "Wil je meedoen aan een interview?",
                name = "meedoen",
                options = [ "ja", "Ja"; "nee", "Nee" ],
                selected = model.Meedoen,
                update = fun meedoen -> onChange (update (MeedoenChanged meedoen) model))
            if model.Meedoen = "ja" then
                Html.p [
                    spacing.mb5
                    prop.text ("Wat leuk dat je mee wilt doen! Vul hieronder je gegevens in, zodat we contact met je op kunnen nemen. "
                        + "We koppelen deze informatie niet aan de errors die je straks invult, die blijven volledig anoniem.")
                ]
                Form.textbox(
                    title = "Hoe heet je?",
                    value = model.Naam,
                    update = fun naam -> onChange (update (NaamChanged naam) model))
                Form.textbox(
                    title = "In welke klas zit je?",
                    explainer = [
                        Html.p [
                            prop.className "is-size-7 mb-2"
                            prop.text "Dan zorgen we dat je samen met een klasgenoot wordt uitgenodigd."
                        ]
                    ],
                    value = model.Klas,
                    update = fun klas -> onChange (update (KlasChanged klas) model))
                Form.textbox(
                    title = "Op welke school zit je?",
                    value = model.School,
                    update = fun school -> onChange (update (SchoolChanged school) model))
                Form.textbox(
                    title = "Wat is je emailadres?",
                    value = model.Email,
                    update = fun email -> onChange (update (EmailChanged email) model))
                Form.radio(
                    title = "Ben je 16 jaar of ouder?",
                    name = "jongerdan16",
                    options = [ "ja", "Ja"; "nee", "Nee" ],
                    selected = model.OuderDan16,
                    update = fun ouderDan16 -> onChange (update (OuderDan16Changed ouderDan16) model))
                if model.OuderDan16 = "nee" then
                    Html.p [
                        spacing.mb5
                        prop.text "Omdat je nog geen 16 bent, moeten je ouders ook toestemming geven om mee te doen aan een interview. Vul hieronder het emailadres van een van je ouders in."
                    ]
                    Form.textbox(
                        title = "Emailadres van een van je ouders",
                        value = model.EmailOuders,
                        update = fun email -> onChange (update (EmailOudersChanged email) model))
            Bulma.button.button [
                color.isPrimary
                prop.disabled (not (Fields.submittable model))
                prop.text "Verder"
            ]
        ]
    ]
