module InterviewForm

open Feliz
open Feliz.Bulma
open Form
open Shared.Api
open Shared.Form
open Shared.Models

module Internal =
    type Model = Interview.Form

    type Msg =
        | Meedoen of Field.Msg<string>
        | Naam of Field.Msg<string>
        | Klas of Field.Msg<string>
        | School of Field.Msg<string>
        | Email of Field.Msg<string>
        | OuderDan16 of Field.Msg<string>
        | EmailOuders of Field.Msg<string>

    let config =
        Interview.Form.config ()

    let init () =
        Interview.Form.init ()

    let update msg (model: Model) =
        match msg with
        | Meedoen msg ->
            { model with Meedoen = Field.update config.Meedoen msg model.Meedoen }
        | Naam msg ->
            { model with Naam = Field.update config.Naam msg model.Naam }
        | Klas msg ->
            { model with Klas = Field.update config.Klas msg model.Klas }
        | School msg ->
            { model with School = Field.update config.School msg model.School }
        | Email msg ->
            { model with Email = Field.update config.Email msg model.Email }
        | OuderDan16 msg ->
            { model with OuderDan16 = Field.update config.OuderDan16 msg model.OuderDan16 }
        | EmailOuders msg ->
            { model with EmailOuders = Field.update config.EmailOuders msg model.EmailOuders }

    let form =
        { SubmitButton = "Opslaan"
          NextButton = Some "Verder"
          CancelButton = Some "Annuleren"
          Update = update
          Validate = Interview.Validate.form config
          ValidateAllFields = Interview.Form.validateAll config }

    let view (model: Model) dispatch = [
        Html.p [
            spacing.mb5
            prop.text ("Met het invullen van dit logboek help je ons al heel veel, maar we gaan ook graag met een aantal "
                + "van jullie in gesprek over de fouten en foutmeldingen die jullie vinden tijdens het programmeren. "
                + "Zo'n interview duurt ongeveer 15 minuten en doe je samen met 1 of 2 klasgenoten.")
        ]
        Form.UI.radio(
            title = "Wil je meedoen aan een interview?",
            name = "meedoen",
            options = [ "ja", "Ja"; "nee", "Nee" ],
            field = model.Meedoen,
            dispatch = (Meedoen >> dispatch))
        if model.Meedoen.Value = Some "ja" then
            Html.p [
                spacing.mb5
                prop.text ("Wat leuk dat je mee wilt doen! Vul hieronder je gegevens in, zodat we contact met je op kunnen nemen. "
                    + "We koppelen deze informatie niet aan de errors die je straks invult, die blijven volledig anoniem.")
            ]
            Form.UI.textbox(
                title = "Hoe heet je?",
                field = model.Naam,
                dispatch = (Naam >> dispatch))
            Form.UI.textbox(
                title = "In welke klas zit je?",
                explainer = [
                    Html.p [
                        prop.className "is-size-7 mb-2"
                        prop.text "Dan zorgen we dat je samen met een klasgenoot wordt uitgenodigd."
                    ]
                ],
                field = model.Klas,
                dispatch = (Klas >> dispatch))
            Form.UI.textbox(
                title = "Op welke school zit je?",
                field = model.School,
                dispatch = (School >> dispatch))
            Form.UI.textbox(
                title = "Wat is je emailadres?",
                field = model.Email,
                dispatch = (Email >> dispatch))
            Form.UI.radio(
                title = "Ben je 16 jaar of ouder?",
                name = "jongerdan16",
                options = [ "ja", "Ja"; "nee", "Nee" ],
                field = model.OuderDan16,
                dispatch = (OuderDan16 >> dispatch))
            if model.OuderDan16.Value = Some "nee" then
                Html.p [
                    spacing.mb5
                    prop.text "Omdat je nog geen 16 bent, moeten je ouders ook toestemming geven om mee te doen aan een interview. Vul hieronder het emailadres van een van je ouders in."
                ]
                Form.UI.textbox(
                    title = "Emailadres van een van je ouders",
                    field = model.EmailOuders,
                    dispatch = (EmailOuders >> dispatch))
    ]

type Model = Form.Model<Internal.Model>
type Msg = Form.Msg<Internal.Msg, Interview.Result, InterviewId>
type ReturnMsg<'msg> = Form.ReturnMsg<Interview.Result, InterviewId, 'msg>

let init () : Model =
    Form.init Internal.init

let update (returnMsg, formMsg) (msg: Msg) (model: Model) =
    Form.update Internal.form returnMsg formMsg msg model

let view (model: Model) (dispatch: Msg -> unit) =
    Form.view Internal.form model dispatch Internal.view
