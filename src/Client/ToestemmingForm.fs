module ToestemmingForm

open Feliz
open Feliz.Bulma
open Form

module Internal =
    type Model =
        { Akkoord: bool }

    type Msg =
        | Akkoord of bool

    let init () =
        { Akkoord = false }

    let update msg model =
        match msg with
        | Akkoord akkoord ->
            { model with Akkoord = akkoord }

    let form =
        { SubmitButton = "Opslaan"
          NextButton = Some "Verder"
          CancelButton = Some "Annuleren"
          Update = update
          Validate = fun fields -> Ok fields.Akkoord
          ValidateAllFields = id
          Submit = fun _ -> async { return Ok () } } // TODO

    let view fields dispatch = [
        Bulma.field.div [
            Bulma.input.labels.checkbox [
                Bulma.input.checkbox [
                    prop.isChecked fields.Akkoord
                    prop.onChange (Akkoord >> dispatch)
                ]
                Bulma.text.span " Ik ga akkoord"
            ]
        ]
    ]

type Model = Form.Model<Internal.Model>
type Msg = Form.Msg<Internal.Msg, bool, unit>
type ReturnMsg = Form.ReturnMsg<bool, unit>

let init () : Model =
    Form.init Internal.init

let update (returnMsg, formMsg) (msg: Msg) (model: Model) =
    Form.update Internal.form returnMsg formMsg msg model

let view (model: Model) (dispatch: Msg -> unit) =
    Form.view Internal.form model dispatch Internal.view
