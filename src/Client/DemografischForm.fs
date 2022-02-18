module DemografischForm

open Feliz
open Feliz.Bulma
open Form
open Shared.Form
open Shared.Models

module Internal =
    type Model = Demografisch.Form

    type Msg =
        | Schoolniveau of Field.Msg<string>
        | ProgrammeerVaardigheid of Field.Msg<int>
        | ProgrammeerErvaring of Field.Msg<int>

    let config =
        Demografisch.Form.config ()

    let init () =
        Demografisch.Form.init ()

    let update msg (model: Model) =
        match msg with
        | Schoolniveau msg ->
            { model with Schoolniveau = Field.update config.Schoolniveau msg model.Schoolniveau }
        | ProgrammeerVaardigheid msg ->
            { model with ProgrammeerVaardigheid = Field.update config.ProgrammeerVaardigheid msg model.ProgrammeerVaardigheid }
        | ProgrammeerErvaring msg ->
            { model with ProgrammeerErvaring = Field.update config.ProgrammeerErvaring msg model.ProgrammeerErvaring }

    let form =
        { SubmitButton = "Opslaan"
          NextButton = Some "Verder"
          CancelButton = Some "Annuleren"
          Update = update
          Validate = Demografisch.Validate.form config
          ValidateAllFields = Demografisch.Form.validateAll config }

    let view (model: Model) (dispatch: Msg -> unit) = [
        Bulma.content [
            Html.p "Fijn dat je mee wilt doen aan dit onderzoek! Voor we verdergaan, willen we nog een paar dingen van je weten."
        ]
        Form.UI.radio(
            title = "Wat is je schoolniveau?",
            name = "schoolniveau",
            options = [ "havo", "Havo"; "vwo", "Vwo" ],
            field = model.Schoolniveau,
            dispatch = (Schoolniveau >> dispatch))
        Form.UI.likert(
            title = "Hoe goed ben je in programmeren?",
            name = "programmeervaardigheid",
            left = "Niet zo goed",
            right = "Heel goed",
            field = model.ProgrammeerVaardigheid,
            dispatch = (ProgrammeerVaardigheid >> dispatch))
        Form.UI.likert(
            title = "Heb je voor deze lessen al eerder geprogrammeerd?",
            name = "programmeerervaring",
            left = "Nooit",
            right = "Heel vaak",
            field = model.ProgrammeerErvaring,
            dispatch = (ProgrammeerErvaring >> dispatch))
    ]

type Model = Form.Model<Internal.Model>
type Msg = Form.Msg<Internal.Msg, Demografisch.Result, unit>
type ReturnMsg<'msg> = Form.ReturnMsg<Demografisch.Result, unit, 'msg>

let init persisted : Model =
    Form.init Internal.init persisted

let update (returnMsg, formMsg) (msg: Msg) (model: Model) =
    Form.update Internal.form returnMsg formMsg msg model

let view (model: Model) (dispatch: Msg -> unit) =
    Form.view Internal.form model dispatch Internal.view
