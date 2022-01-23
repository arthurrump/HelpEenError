module DemografischForm

type Fields =
    { Schoolniveau: string
      ProgrammeerVaardigheid: int
      ProgrammeerErvaring: int }

module Fields =
    let submittable fields =
        fields.Schoolniveau <> "" &&
        fields.ProgrammeerVaardigheid <> 0 &&
        fields.ProgrammeerErvaring <> 0

type Msg =
    | SchoolniveauChanged of string
    | ProgrammeerVaardigheidChanged of int
    | ProgrammeerErvaringChanged of int

let init () =
    { Schoolniveau = ""
      ProgrammeerVaardigheid = 0
      ProgrammeerErvaring = 0 }

let update msg model =
    match msg with
    | SchoolniveauChanged schoolniveau ->
        { model with Schoolniveau = schoolniveau }
    | ProgrammeerVaardigheidChanged programmeerVaardigheid ->
        { model with ProgrammeerVaardigheid = programmeerVaardigheid }
    | ProgrammeerErvaringChanged programmeerErvaring ->
        { model with ProgrammeerErvaring = programmeerErvaring }

open Feliz
open Feliz.Bulma
open FormHelpers

let view model onChange onSubmit =
    Html.form [
        prop.onSubmit (fun ev ->
            ev.preventDefault ()
            onSubmit model
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
                selected = model.Schoolniveau,
                update = fun schoolniveau -> onChange (update (SchoolniveauChanged schoolniveau) model))
            Form.likert(
                title = "Hoe goed ben je in programmeren?",
                name = "programmeervaardigheid",
                left = "Heel slecht",
                right = "Heel goed",
                value = model.ProgrammeerVaardigheid,
                update = fun pv -> onChange (update (ProgrammeerVaardigheidChanged pv) model))
            Form.likert(
                title = "Heb je voor deze lessen al eerder geprogrammeerd?",
                name = "programmeerervaring",
                left = "Nooit",
                right = "Heel vaak",
                value = model.ProgrammeerErvaring,
                update = fun pe -> onChange (update (ProgrammeerErvaringChanged pe) model))
            Bulma.button.button [
                color.isPrimary
                prop.disabled (not (Fields.submittable model))
                prop.text "Verder"
            ]
        ]
    ]
