module AfsluitingForm

open Feliz
open Feliz.Bulma
open Form
open Shared.Form
open Shared.Models

module Internal =
    type Model = Afsluiting.Form

    type Msg =
        | OpgelostVanLogboek of Field.Msg<int>
        | OpeglostInVergelijking of Field.Msg<int>
        | TijdBesteed of Field.Msg<int>

    let config =
        Afsluiting.Form.config ()

    let init () =
        Afsluiting.Form.init ()

    let update msg (model: Model) =
        match msg with
        | OpgelostVanLogboek msg ->
            { model with OpgelostVanLogboek = Field.update config.OpgelostVanLogboek msg model.OpgelostVanLogboek }
        | OpeglostInVergelijking msg ->
            { model with OpgelostInVergelijking = Field.update config.OpgelostInVergelijking msg model.OpgelostInVergelijking }
        | TijdBesteed msg ->
            { model with TijdBesteed = Field.update config.TijdBesteed msg model.TijdBesteed }

    let form =
        { SubmitButton = "Opslaan"
          NextButton = Some "Opslaan en afsluiten"
          CanCombineSubmitNext = true
          CancelButton = Some "Annuleren"
          Update = update
          Validate = Afsluiting.Validate.form config
          ValidateAllFields = Afsluiting.Form.validateAll config
          InitFields = init }

    let view (model: Model) (dispatch: Msg -> unit) = [
        Bulma.content [
            Html.p "Tijdens deze les heeft je docent jou niet geholpen bij het oplossen van programmeerfouten en geen vragen beantwoord over programmeren. Je moest dus zelfstandiger aan het werk. Wij zijn benieuwd hoe die zelfstandigheid invloed heeft gehad op de manier waarop jij met fouten bent omgegaan."
        ]
        Form.UI.likert(
            title = "Hoeveel van de fouten in het logboek heb je kunnen oplossen?",
            name = "opgelostvanlogboek",
            left = "Geen",
            right = "Allemaal",
            field = model.OpgelostVanLogboek,
            dispatch = (OpgelostVanLogboek >> dispatch))
        Form.UI.likert(
            title = "Hoeveel fouten heb je in deze les opgelost in vergelijking met andere lessen?",
            name = "opgelostinvergelijking",
            left = "Veel minder",
            right = "Veel meer",
            field = model.OpgelostInVergelijking,
            dispatch = (OpeglostInVergelijking >> dispatch))
        Form.UI.likert(
            title = "Hoeveel tijd heb je in deze les besteed aan het oplossen van fouten in vergelijking met andere lessen?",
            name = "tijdbesteed",
            left = "Veel minder",
            right = "Veel meer",
            field = model.TijdBesteed,
            dispatch = (TijdBesteed >> dispatch))
    ]

type Model = Form.Model<Internal.Model>
type Msg = Form.Msg<Internal.Msg, Afsluiting.Result, unit>
type ReturnMsg<'msg> = Form.ReturnMsg<Afsluiting.Result, unit, 'msg>

let init persisted : Model =
    Form.init Internal.form persisted

let update (returnMsg, formMsg) (msg: Msg) (model: Model) =
    Form.update Internal.form returnMsg formMsg msg model

let view (model: Model) (dispatch: Msg -> unit) =
    Form.view Internal.form model dispatch Internal.view
