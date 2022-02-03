module LogboekForm

open Feliz
open Feliz.Bulma
open Form
open Shared.Form
open Shared.Library
open Shared.Models

module Internal =
    type Model = Logboek.Form

    type Msg =
        | Type of Field.Msg<string>
        | FoutmeldingMelding of Field.Msg<string>
        | OnverwachtGedragBeschrijving of Field.Msg<string>
        | OnverwachtGedragVerwachting of Field.Msg<string>
        | Vervolgactie of Field.Msg<string>

    let init () =
        Logboek.Form.init ()

    let update msg (model: Model) =
        match msg with
        | Type msg ->
            { model with Type = Field.update msg model.Type }
        | FoutmeldingMelding msg ->
            { model with FoutmeldingMelding = Field.update msg model.FoutmeldingMelding }
        | OnverwachtGedragBeschrijving msg ->
            { model with OnverwachtGedragBeschrijving = Field.update msg model.OnverwachtGedragBeschrijving }
        | OnverwachtGedragVerwachting msg ->
            { model with OnverwachtGedragVerwachting = Field.update msg model.OnverwachtGedragVerwachting }
        | Vervolgactie msg ->
            { model with Vervolgactie = Field.update msg model.Vervolgactie }

    let form =
        { SubmitButton = "Toevoegen"
          NextButton = None
          CancelButton = Some "Annuleren"
          Update = update
          Validate = Logboek.Validate.form
          ValidateAllFields = Logboek.Form.validateAll
          Submit = fun _ -> async { return Ok () } } // TODO

    let view (model: Model) (dispatch: Msg -> unit) = [
        Form.UI.radio(
            title = "Wat gaat er mis?",
            name = "type",
            options =
                [ "foutmelding", "Ik krijg een foutmelding"
                  "onverwacht-gedrag", "Het programma werkt niet zoals ik verwacht had" ],
            field = model.Type,
            dispatch = (Type >> dispatch))
        if model.Type.Value = Some "foutmelding" then
            Form.UI.textarea (
                title = "Wat is de foutmelding?",
                field = model.FoutmeldingMelding,
                dispatch = (FoutmeldingMelding >> dispatch))
        if model.Type.Value = Some "onverwacht-gedrag" then
            Form.UI.textarea(
                title = "Wat verwachtte je dat het programma zou doen?",
                field = model.OnverwachtGedragVerwachting,
                dispatch = (OnverwachtGedragVerwachting >> dispatch))
            Form.UI.textarea(
                title = "Wat doet het programma?",
                field = model.OnverwachtGedragBeschrijving,
                dispatch = (OnverwachtGedragBeschrijving >> dispatch))
        if Field.validate model.Type |> Result.isOk then
            Form.UI.textarea(
                title = "Wat ga je doen om de fout op te lossen?",
                field = model.Vervolgactie,
                dispatch = (Vervolgactie >> dispatch))
    ]

type Model = Form.Model<Internal.Model>
type Msg = Form.Msg<Internal.Msg, Logboek.Result, unit>
type ReturnMsg = Form.ReturnMsg<Logboek.Result, unit>

let init () : Model =
    Form.init Internal.init

let update (returnMsg, formMsg) (msg: Msg) (model: Model) =
    Form.update Internal.form returnMsg formMsg msg model

let view (model: Model) (dispatch: Msg -> unit) =
    Form.view Internal.form model dispatch Internal.view
