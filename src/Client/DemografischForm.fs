module DemografischForm

open Form
open Elmish
open Shared.Form
open Shared.Library
open Shared.Models
open Validus

type Model =
    { Form: Demografisch.Form
      Errors: string list
      SubmitDisabled: bool }

type Msg =
    | Schoolniveau of Field.Msg<string>
    | ProgrammeerVaardigheid of Field.Msg<int>
    | ProgrammeerErvaring of Field.Msg<int>
    | Submit

let init () : Model =
    { Form = Demografisch.Form.init ()
      Errors = []
      SubmitDisabled = false }

let update submit msg (model: Model) =
    let model = { model with SubmitDisabled = false }
    match msg with
    | Schoolniveau msg ->
        { model with Form = { model.Form with Schoolniveau = Field.update msg model.Form.Schoolniveau } }, Cmd.none
    | ProgrammeerVaardigheid msg ->
        { model with Form = { model.Form with ProgrammeerVaardigheid = Field.update msg model.Form.ProgrammeerVaardigheid } }, Cmd.none
    | ProgrammeerErvaring msg ->
        { model with Form = { model.Form with ProgrammeerErvaring = Field.update msg model.Form.ProgrammeerErvaring } }, Cmd.none
    | Submit ->
        let model = { model with Form = Demografisch.Form.validateAll model.Form }
        match Demografisch.Validate.form model.Form with
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
            Html.p [
                spacing.mb5
                prop.text "Fijn dat je mee wilt doen aan dit onderzoek! Voor we verdergaan, willen we nog een paar dingen van je weten."
            ]
            Form.radio(
                title = "Wat is je schoolniveau?",
                name = "schoolniveau",
                options = [ "havo", "Havo"; "vwo", "Vwo" ],
                field = model.Form.Schoolniveau,
                dispatch = (Schoolniveau >> dispatch))
            Form.likert(
                title = "Hoe goed ben je in programmeren?",
                name = "programmeervaardigheid",
                left = "Heel slecht",
                right = "Heel goed",
                field = model.Form.ProgrammeerVaardigheid,
                dispatch = (ProgrammeerVaardigheid >> dispatch))
            Form.likert(
                title = "Heb je voor deze lessen al eerder geprogrammeerd?",
                name = "programmeerervaring",
                left = "Nooit",
                right = "Heel vaak",
                field = model.Form.ProgrammeerErvaring,
                dispatch = (ProgrammeerErvaring >> dispatch))
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
