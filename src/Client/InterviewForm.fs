module InterviewForm

open Form
open Elmish
open Shared.Form
open Shared.Models
open Validus

type Model =
    { Form: Interview.Form
      Errors: string list
      SubmitDisabled: bool }

let init () =
    { Form = Interview.Form.init ()
      Errors = []
      SubmitDisabled = false }

type Msg =
    | Meedoen of Field.Msg<string>
    | Naam of Field.Msg<string>
    | Klas of Field.Msg<string>
    | School of Field.Msg<string>
    | Email of Field.Msg<string>
    | OuderDan16 of Field.Msg<string>
    | EmailOuders of Field.Msg<string>
    | Submit

let update submit msg (model: Model) =
    let model = { model with SubmitDisabled = false }
    match msg with
    | Meedoen msg ->
        { model with Form = { model.Form with Meedoen = Field.update msg model.Form.Meedoen } }, Cmd.none
    | Naam msg ->
        { model with Form = { model.Form with Naam = Field.update msg model.Form.Naam } }, Cmd.none
    | Klas msg ->
        { model with Form = { model.Form with Klas = Field.update msg model.Form.Klas } }, Cmd.none
    | School msg ->
        { model with Form = { model.Form with School = Field.update msg model.Form.School } }, Cmd.none
    | Email msg ->
        { model with Form = { model.Form with Email = Field.update msg model.Form.Email } }, Cmd.none
    | OuderDan16 msg ->
        { model with Form = { model.Form with OuderDan16 = Field.update msg model.Form.OuderDan16 } }, Cmd.none
    | EmailOuders msg ->
        { model with Form = { model.Form with EmailOuders = Field.update msg model.Form.EmailOuders } }, Cmd.none
    | Submit ->
        let model = { model with Form = Interview.Form.validateAll model.Form }
        match Interview.Validate.form model.Form with
        | Ok result ->
            { model with Errors = [] }, Cmd.ofMsg (submit result)
        | Error err ->
            { model with
                SubmitDisabled = true
                Errors = err |> ValidationErrors.toList }, Cmd.none

open Feliz
open Feliz.Bulma

let view (model: Model) dispatch =
    Html.form [
        prop.onSubmit (fun ev ->
            ev.preventDefault ()
            dispatch Submit
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
                field = model.Form.Meedoen,
                dispatch = (Meedoen >> dispatch))
            if model.Form.Meedoen.Value = Some "ja" then
                Html.p [
                    spacing.mb5
                    prop.text ("Wat leuk dat je mee wilt doen! Vul hieronder je gegevens in, zodat we contact met je op kunnen nemen. "
                        + "We koppelen deze informatie niet aan de errors die je straks invult, die blijven volledig anoniem.")
                ]
                Form.textbox(
                    title = "Hoe heet je?",
                    field = model.Form.Naam,
                    dispatch = (Naam >> dispatch))
                Form.textbox(
                    title = "In welke klas zit je?",
                    explainer = [
                        Html.p [
                            prop.className "is-size-7 mb-2"
                            prop.text "Dan zorgen we dat je samen met een klasgenoot wordt uitgenodigd."
                        ]
                    ],
                    field = model.Form.Klas,
                    dispatch = (Klas >> dispatch))
                Form.textbox(
                    title = "Op welke school zit je?",
                    field = model.Form.School,
                    dispatch = (School >> dispatch))
                Form.textbox(
                    title = "Wat is je emailadres?",
                    field = model.Form.Email,
                    dispatch = (Email >> dispatch))
                Form.radio(
                    title = "Ben je 16 jaar of ouder?",
                    name = "jongerdan16",
                    options = [ "ja", "Ja"; "nee", "Nee" ],
                    field = model.Form.OuderDan16,
                    dispatch = (OuderDan16 >> dispatch))
                if model.Form.OuderDan16.Value = Some "nee" then
                    Html.p [
                        spacing.mb5
                        prop.text "Omdat je nog geen 16 bent, moeten je ouders ook toestemming geven om mee te doen aan een interview. Vul hieronder het emailadres van een van je ouders in."
                    ]
                    Form.textbox(
                        title = "Emailadres van een van je ouders",
                        field = model.Form.EmailOuders,
                        dispatch = (EmailOuders >> dispatch))
            Bulma.button.button [
                color.isPrimary
                prop.disabled model.SubmitDisabled
                prop.text "Verder"
            ]
            if not (List.isEmpty model.Errors) then
                Bulma.notification [
                    color.isDanger
                    spacing.mt5
                    prop.children [
                        Html.ul [
                            for error in model.Errors |> Seq.rev ->
                                Html.li [
                                    prop.text error
                                ]
                        ]
                    ]
                ]
        ]
    ]
