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

    let init () =
        Demografisch.Form.init ()

    let update msg (model: Model) =
        match msg with
        | Schoolniveau msg ->
            { model with Schoolniveau = Field.update msg model.Schoolniveau }
        | ProgrammeerVaardigheid msg ->
            { model with ProgrammeerVaardigheid = Field.update msg model.ProgrammeerVaardigheid }
        | ProgrammeerErvaring msg ->
            { model with ProgrammeerErvaring = Field.update msg model.ProgrammeerErvaring }

    let form =
        { SubmitButton = "Opslaan"
          NextButton = Some "Verder"
          CancelButton = Some "Annuleren"
          Update = update
          Validate = Demografisch.Validate.form
          ValidateAllFields = Demografisch.Form.validateAll
          Submit = fun _ -> async { return Ok () } } // TODO

    let view (model: Model) (dispatch: Msg -> unit) = [
        Html.p [
            spacing.mb5
            prop.text "Fijn dat je mee wilt doen aan dit onderzoek! Voor we verdergaan, willen we nog een paar dingen van je weten."
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
            left = "Heel slecht",
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
type ReturnMsg = Form.ReturnMsg<Demografisch.Result, unit>

let init () : Model =
    Form.init Internal.init

let update (returnMsg, formMsg) (msg: Msg) (model: Model) =
    Form.update Internal.form returnMsg formMsg msg model

let view (model: Model) (dispatch: Msg -> unit) =
    Form.view Internal.form model dispatch Internal.view
