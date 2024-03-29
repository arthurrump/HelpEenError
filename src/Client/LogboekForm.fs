module LogboekForm

open Feliz
open Feliz.Bulma
open Form
open Shared.Api
open Shared.Form
open Shared.Library
open Shared.Models

module Internal =
    type Model =
        { Form: Logboek.Form
          FoutmeldingPlaceholder: string }

    module Model =
        let getForm model = model.Form
        let updateForm f model = { model with Form = f model.Form }

    type Msg =
        | Type of Field.Msg<string>
        | FoutmeldingMelding of Field.Msg<string>
        | OnverwachtGedragBeschrijving of Field.Msg<string>
        | OnverwachtGedragVerwachting of Field.Msg<string>
        | Vervolgactie of Field.Msg<string>

    let config =
        Logboek.Form.config ()

    let init =
        let random = System.Random ()
        let foutmeldingPlaceholders =
            [| "  File \"main.py\", line 1\n    print( 0 / 0 ))\n                  ^\nSyntaxError: invalid syntax"
               "Traceback (most recent call last):\n  File \"main.py\", line 2, in <module>\n    print(y)\nNameError: name 'y' is not defined"
               "  File \"main.py\", line 1\n    x = 0, y = 0\n        ^\nSyntaxError: cannot assign to literal"
               "  File \"main.py\", line 4\n    print(\"It's false!\")\n    ^\nIndentationError: expected an indented block"
               "Traceback (most recent call last):\n  File \"main.py\", line 2, in <module>\n    print(math.acos(\"1\"))\nTypeError: must be real number, not str"
               "Traceback (most recent call last):\n  File \"main.py\", line 1, in <module>\n    1/0\nZeroDivisionError: division by zero" |]
        fun () ->
            { Form = Logboek.Form.init ()
              FoutmeldingPlaceholder = foutmeldingPlaceholders[random.Next(foutmeldingPlaceholders.Length)] }

    let updateForm msg (form: Logboek.Form) =
        match msg with
        | Type msg ->
            { form with Type = Field.update config.Type msg form.Type }
        | FoutmeldingMelding msg ->
            { form with FoutmeldingMelding = Field.update config.FoutmeldingMelding msg form.FoutmeldingMelding }
        | OnverwachtGedragBeschrijving msg ->
            { form with OnverwachtGedragBeschrijving = Field.update config.OnverwachtGedragBeschrijving msg form.OnverwachtGedragBeschrijving }
        | OnverwachtGedragVerwachting msg ->
            { form with OnverwachtGedragVerwachting = Field.update config.OnverwachtGedragVerwachting msg form.OnverwachtGedragVerwachting }
        | Vervolgactie msg ->
            { form with Vervolgactie = Field.update config.Vervolgactie msg form.Vervolgactie }

    let update msg (model: Model) =
        { model with Form = updateForm msg model.Form }

    let form =
        { SubmitButton = "Toevoegen"
          NextButton = Some "Afsluiten"
          CanCombineSubmitNext = false
          CancelButton = Some "Annuleren"
          Update = update
          Validate = Model.getForm >> Logboek.Validate.form config
          ValidateAllFields = Model.updateForm (Logboek.Form.validateAll config)
          InitFields = init }

    let view (model: Model) (dispatch: Msg -> unit) = [
        let form = model.Form
        Bulma.content [
            Html.p [
                Html.text "Op deze pagina kun je bijhouden welke fouten je tegenkomt tijdens deze les. Je hoeft alleen fouten toe te voegen "
                Html.text "waarvan je niet direct weet wat je ermee moet doen. We kijken naar twee soorten fouten: je kunt een foutmelding krijgen "
                Html.text "als Python vindt dat er iets niet klopt aan de code (te herkennen aan de rode tekst, bijvoorbeeld in de Console van Replit), "
                Html.text "of het kan zijn dat je programma wel uitgevoerd wordt, maar niet doet wat de bedoeling was."
            ]
            Html.p [
                Html.text "Klik aan het eind van de les op de knop "
                Html.em "Afsluiten"
                Html.text " om de laatste vragen te beantwoorden en het logboek af te sluiten."
            ]
        ]
        Form.UI.radio(
            title = "Wat gaat er mis?",
            name = "type",
            options =
                [ "foutmelding", "Ik krijg een foutmelding"
                  "onverwacht-gedrag", "Het programma werkt niet zoals ik verwacht" ],
            field = form.Type,
            dispatch = (Type >> dispatch))
        if form.Type.Value = Some "foutmelding" then
            Form.UI.textarea (
                title = "Wat is de foutmelding?",
                field = form.FoutmeldingMelding,
                dispatch = (FoutmeldingMelding >> dispatch),
                props = [ text.isFamilyMonospace; prop.rows 5 ],
                placeholder = "Voorbeeld:\n" + model.FoutmeldingPlaceholder,
                explainer = [
                    Html.p [
                        prop.className "is-size-7 mb-2"
                        prop.children [
                            Html.text "Kopieer de foutmelding en plak die hier. Gebruik je Replit? Let dan op: selecteer de foutmelding "
                            Html.text "in de Console en gebruik de rechtermuisknop om te kopiëren. De sneltoets Ctrl+C werkt niet in de Console."
                        ]
                    ]
                ])
        if form.Type.Value = Some "onverwacht-gedrag" then
            Form.UI.textarea(
                title = "Wat verwachtte je dat het programma zou doen?",
                field = form.OnverwachtGedragVerwachting,
                dispatch = (OnverwachtGedragVerwachting >> dispatch))
            Form.UI.textarea(
                title = "Wat doet het programma?",
                field = form.OnverwachtGedragBeschrijving,
                dispatch = (OnverwachtGedragBeschrijving >> dispatch))
        if Field.validate config.Type form.Type |> Result.isOk then
            Form.UI.textarea(
                title = "Wat ga je doen om de fout op te lossen?",
                placeholder = "Bijvoorbeeld een klasgenoot om hulp vragen, de code nog eens rustig doorlezen, in Fundament opzoeken hoe de code eruit moet zien, ...",
                field = form.Vervolgactie,
                dispatch = (Vervolgactie >> dispatch))
            Bulma.content [
                Html.p [
                    Html.text "Klik op de knop "; Html.i "Toevoegen"; Html.text " om deze fout toe te voegen aan het logboek. Daarna "
                    Html.text "kun je deze vragen opnieuw beantwoorden bij de volgende fout die je tegenkomt."
                ]
            ]
    ]

type Model = Form.Model<Internal.Model>
type Msg = Form.Msg<Internal.Msg, Logboek.Result, LogId>
type ReturnMsg<'msg> = Form.ReturnMsg<Logboek.Result, LogId, 'msg>

let init persisted : Model =
    Form.init Internal.form persisted

let update (returnMsg, formMsg) (msg: Msg) (model: Model) =
    Form.update Internal.form returnMsg formMsg msg model

let view (model: Model) (dispatch: Msg -> unit) =
    Form.view Internal.form model dispatch Internal.view
