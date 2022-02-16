module LogboekForm

open Feliz
open Feliz.Bulma
open Form
open Shared.Api
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

    let config =
        Logboek.Form.config ()

    let init () =
        Logboek.Form.init ()

    let update msg (model: Model) =
        match msg with
        | Type msg ->
            { model with Type = Field.update config.Type msg model.Type }
        | FoutmeldingMelding msg ->
            { model with FoutmeldingMelding = Field.update config.FoutmeldingMelding msg model.FoutmeldingMelding }
        | OnverwachtGedragBeschrijving msg ->
            { model with OnverwachtGedragBeschrijving = Field.update config.OnverwachtGedragBeschrijving msg model.OnverwachtGedragBeschrijving }
        | OnverwachtGedragVerwachting msg ->
            { model with OnverwachtGedragVerwachting = Field.update config.OnverwachtGedragVerwachting msg model.OnverwachtGedragVerwachting }
        | Vervolgactie msg ->
            { model with Vervolgactie = Field.update config.Vervolgactie msg model.Vervolgactie }

    let form =
        { SubmitButton = "Toevoegen"
          NextButton = None
          CancelButton = Some "Annuleren"
          Update = update
          Validate = Logboek.Validate.form config
          ValidateAllFields = Logboek.Form.validateAll config }

    let view (model: Model) (dispatch: Msg -> unit) = [
        Form.UI.radio(
            title = "Wat gaat er mis?",
            name = "type",
            options =
                [ "foutmelding", "Ik krijg een foutmelding"
                  "onverwacht-gedrag", "Het programma werkt niet zoals ik verwacht" ],
            field = model.Type,
            dispatch = (Type >> dispatch))
        if model.Type.Value = Some "foutmelding" then
            Form.UI.textarea (
                title = "Wat is de foutmelding?",
                field = model.FoutmeldingMelding,
                dispatch = (FoutmeldingMelding >> dispatch),
                props = [ text.isFamilyMonospace ],
                placeholder = "  File \"main.py\", line 1\n    print( 0 / 0 ))\n                  ^\nSyntaxError: invalid syntax")
        if model.Type.Value = Some "onverwacht-gedrag" then
            Form.UI.textarea(
                title = "Wat verwachtte je dat het programma zou doen?",
                field = model.OnverwachtGedragVerwachting,
                dispatch = (OnverwachtGedragVerwachting >> dispatch))
            Form.UI.textarea(
                title = "Wat doet het programma?",
                field = model.OnverwachtGedragBeschrijving,
                dispatch = (OnverwachtGedragBeschrijving >> dispatch))
        if Field.validate config.Type model.Type |> Result.isOk then
            Form.UI.textarea(
                title = "Wat ga je doen om de fout op te lossen?",
                field = model.Vervolgactie,
                dispatch = (Vervolgactie >> dispatch))
    ]

type Model = Form.Model<Internal.Model>
type Msg = Form.Msg<Internal.Msg, Logboek.Result, LogId>
type ReturnMsg<'msg> = Form.ReturnMsg<Logboek.Result, LogId, 'msg>

let init persisted : Model =
    Form.init Internal.init persisted

let update (returnMsg, formMsg) (msg: Msg) (model: Model) =
    Form.update Internal.form returnMsg formMsg msg model

let view (model: Model) (dispatch: Msg -> unit) =
    Form.view Internal.form model dispatch Internal.view
