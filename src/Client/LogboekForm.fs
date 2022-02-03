module LogboekForm

open Form
open Elmish
open Shared.Form
open Shared.Models
open Validus

type Model =
    { Form: Logboek.Form
      Errors: string list
      SubmitDisabled: bool }

type Msg =
    | Type of Field.Msg<string>
    | FoutmeldingMelding of Field.Msg<string>
    | OnverwachtGedragBeschrijving of Field.Msg<string>
    | OnverwachtGedragVerwachting of Field.Msg<string>
    | Vervolgactie of Field.Msg<string>
    | Submit

let init () : Model =
    { Form = Logboek.Form.init ()
      Errors = []
      SubmitDisabled = false }

let update submit msg (model: Model) =
    let model = { model with SubmitDisabled = false }
    match msg with
    | Type msg ->
        { model with Form = { model.Form with Type = Field.update msg model.Form.Type } }, Cmd.none
    | FoutmeldingMelding msg ->
        { model with Form = { model.Form with FoutmeldingMelding = Field.update msg model.Form.FoutmeldingMelding } }, Cmd.none
    | OnverwachtGedragBeschrijving msg ->
        { model with Form = { model.Form with OnverwachtGedragBeschrijving = Field.update msg model.Form.OnverwachtGedragBeschrijving } }, Cmd.none
    | OnverwachtGedragVerwachting msg ->
        { model with Form = { model.Form with OnverwachtGedragVerwachting = Field.update msg model.Form.OnverwachtGedragVerwachting } }, Cmd.none
    | Vervolgactie msg ->
        { model with Form = { model.Form with Vervolgactie = Field.update msg model.Form.Vervolgactie } }, Cmd.none
    | Submit ->
        let model = { model with Form = Logboek.Form.validateAll model.Form }
        match Logboek.Validate.form model.Form with
        | Ok result ->
            { model with Errors = [] }, Cmd.ofMsg (submit result)
        | Error err ->
            { model with
                SubmitDisabled = true
                Errors = err |> ValidationErrors.toList }, Cmd.none

open Feliz
open Feliz.Bulma

let view (model: Model) (dispatch: Msg -> unit) =
    Html.form [
        prop.onSubmit (fun ev ->
            ev.preventDefault ()
            dispatch Submit
        )
        prop.children [

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
