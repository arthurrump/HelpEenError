module Form

open Elmish
open Feliz
open Feliz.Bulma
open Validus

open Shared.Form

type Form<'fields, 'fieldsMsg, 'result, 'response> =
    { SubmitButton: string
      NextButton: string option
      CancelButton: string option
      Update: 'fieldsMsg -> 'fields -> 'fields
      Validate: 'fields -> Result<'result, ValidationErrors>
      ValidateAllFields: 'fields -> 'fields }

module Form =
    type Model<'fields> =
        { Fields: 'fields
          OldFields: 'fields option
          Errors: string list
          CombineSubmitNext: bool
          IsDirty: bool
          IsSubmitting: bool
          IsSubmitDisabled: bool }

    type Msg<'fieldsMsg, 'result, 'response> =
        | FieldsMsg of 'fieldsMsg
        | SubmissionResponseReceived of emitNext: bool * 'result * Result<'response, string>
        | ClickedSubmitAndNext
        | ClickedSubmit
        | ClickedCancel
        | ClickedNext

    type ReturnMsg<'result, 'response, 'msg> =
        | Submit of 'result * onResponse: (Result<'response, string> -> 'msg)
        | Submitted of 'result * 'response
        | Next

    let init initFields =
        { Fields = initFields ()
          OldFields = None
          Errors = []
          CombineSubmitNext = true
          IsDirty = false
          IsSubmitting = false
          IsSubmitDisabled = false }

    let private storeOldFields model =
        if model.OldFields = None then
            { model with OldFields = Some model.Fields }
        else
            model

    let private canCancel model =
        model.IsDirty
        && model.OldFields |> Option.isSome
        && Some model.Fields <> model.OldFields
    let private cancel model =
        match model.OldFields with
        | Some old ->
            { model with
                Fields = old
                OldFields = None
                IsDirty = false
                Errors = [] }
        | None ->
            model

    let update
            (form: Form<'fields, 'fieldsMsg, 'result, 'response>)
            (returnMsg: ReturnMsg<'result, 'response, 'msg> -> 'msg)
            (internalMsg: Msg<'fieldsMsg, 'result, 'response> -> 'msg)
            (msg: Msg<'fieldsMsg, 'result, 'response>)
            (model: Model<'fields>) : Model<'fields> * Cmd<'msg> =
        match msg with
        | FieldsMsg fieldsMsg ->
            let model = storeOldFields model
            { model with
                IsDirty = true
                IsSubmitDisabled = false
                Fields = form.Update fieldsMsg model.Fields }, Cmd.none
        | ClickedSubmit | ClickedSubmitAndNext ->
            let model = { model with Fields = form.ValidateAllFields model.Fields }
            match form.Validate model.Fields with
            | Ok result ->
                let cmd =
                    let onResponse resp = internalMsg (SubmissionResponseReceived ((msg = ClickedSubmitAndNext), result, resp))
                    Cmd.ofMsg (returnMsg (Submit (result, onResponse)))
                { model with
                    Errors = []
                    IsSubmitting = true }, cmd
            | Error err ->
                { model with
                    IsSubmitDisabled = true
                    Errors = err |> ValidationErrors.toList }, Cmd.none
        | SubmissionResponseReceived (emitNext, result, Ok response) ->
            let cmd = Cmd.batch [
                Cmd.ofMsg (returnMsg (Submitted (result, response)))
                if emitNext then Cmd.ofMsg (returnMsg Next) else Cmd.none ]
            { model with
                OldFields = None
                IsDirty = false
                CombineSubmitNext = false
                IsSubmitting = false }, cmd
        | SubmissionResponseReceived (_, _, Error err) ->
            { model with
                IsSubmitting = false
                Errors = [ err ] }, Cmd.none
        | ClickedCancel ->
            cancel model, Cmd.none
        | ClickedNext ->
            cancel model, Cmd.ofMsg (returnMsg Next)

    let private showError field =
        match field.Error with
        | Some error ->
            [ Bulma.help [ color.isDanger; prop.text error ] ]
        | None ->
            []

    type UI =
        static member titleDiv(
                ?title: string,
                ?explainer: seq<Fable.React.ReactElement>,
                ?children: seq<Fable.React.ReactElement>) =
            Bulma.field.div [
                spacing.mb5
                prop.children [
                    match title with Some t -> Bulma.label t | _ -> ()
                    yield! (defaultArg explainer Seq.empty)
                    yield! (defaultArg children Seq.empty)
                ]
            ]

        static member textbox(
                field: Field<string>,
                dispatch: Field.Msg<string> -> unit,
                ?title: string,
                ?explainer: seq<Fable.React.ReactElement>,
                ?placeholder: string) =
            UI.titleDiv(?title = title, ?explainer = explainer, children = [
                Bulma.control.div [
                    Bulma.input.text [
                        if Option.isSome field.Error then color.isDanger
                        prop.placeholder (defaultArg placeholder "")
                        prop.value (Option.defaultValue "" field.Value)
                        prop.onTextChange (Field.Update >> dispatch)
                        prop.onBlur (fun _ -> dispatch Field.Validate)
                    ]
                ]
                yield! showError field
            ])

        static member textarea(
                field: Field<string>,
                dispatch: Field.Msg<string> -> unit,
                ?title: string,
                ?explainer: seq<Fable.React.ReactElement>,
                ?rows: int,
                ?placeholder: string) =
            UI.titleDiv(?title = title, ?explainer = explainer, children = [
                Bulma.control.div [
                    Bulma.textarea [
                        if Option.isSome field.Error then color.isDanger
                        prop.placeholder (defaultArg placeholder "")
                        prop.value (Option.defaultValue "" field.Value)
                        match rows with Some rows -> prop.rows rows | None -> ()
                        prop.onTextChange (Field.Update >> dispatch)
                        prop.onBlur (fun _ -> dispatch Field.Validate)
                    ]
                ]
                yield! showError field
            ])

        static member radio(
                name: string,
                options: (string * string) list,
                field: Field<string>,
                dispatch: Field.Msg<string> -> unit,
                ?title: string,
                ?explainer: seq<Fable.React.ReactElement>) =
            UI.titleDiv(?title = title, ?explainer = explainer, children = [
                Html.ul [
                    for (value, item) in options ->
                        Html.li [
                            Bulma.input.labels.radio [
                                Bulma.input.radio [
                                    prop.name name
                                    prop.value value
                                    prop.isChecked ((field.Value = Some value))
                                    prop.onCheckedChange (fun ch ->
                                        if ch then dispatch (Field.Update value))
                                    prop.onBlur (fun _ -> dispatch Field.Validate)
                                    spacing.mr2
                                ]
                                Html.text item
                            ]
                        ]
                ]
                yield! showError field
            ])

        static member likert(
                name: string,
                field: Field<int>,
                dispatch: Field.Msg<int> -> unit,
                ?title: string,
                ?explainer: seq<Fable.React.ReactElement>,
                ?left: string,
                ?right: string,
                ?min: int,
                ?max: int,
                ?step: int) =
            let min = defaultArg min 1
            let max = defaultArg max 5
            let step = defaultArg step 1
            UI.titleDiv(?title = title, ?explainer = explainer, children = [
                Html.div [
                    spacing.mb2
                    prop.children [
                        match left with
                        | Some l ->
                            Html.span [
                                Html.i [ prop.className "fas fa-arrow-left mr-2" ]
                                Html.text l
                            ]
                        | _ -> ()
                        match right with
                        | Some r ->
                            Html.span [
                                prop.style [ style.floatStyle.right ]
                                prop.children [
                                    Html.text r
                                    Html.i [ prop.className "fas fa-arrow-right ml-2" ]
                                ]
                            ]
                        | _ -> ()
                    ]
                ]
                Html.div [
                    color.hasBackgroundGreyLighter
                    prop.style [
                        style.height (length.em 3)
                        style.display.flex
                        style.flexDirection.row
                        style.flexWrap.nowrap
                        style.justifyContent.spaceAround
                        style.alignItems.center
                    ]
                    prop.children [
                        for i in min .. step .. max ->
                            Bulma.input.radio [
                                prop.name name
                                prop.value i
                                prop.isChecked ((field.Value = Some i))
                                prop.onCheckedChange (fun ch -> if ch then dispatch (Field.Update i))
                                prop.onBlur (fun _ -> dispatch Field.Validate)
                            ]
                    ]
                ]
                yield! showError field
            ])

    let view (form: Form<'fields, 'fieldsMsg, 'result, 'response>) (model: Model<'fields>) (dispatch: Msg<'fieldsMsg, 'result, 'response> -> unit) (children: 'fields -> ('fieldsMsg -> unit) -> #seq<Fable.React.ReactElement>) =
        Html.form [
            prop.onSubmit (fun ev -> ev.preventDefault ())
            prop.children [
                yield! children model.Fields (FieldsMsg >> dispatch)
                Bulma.field.div [
                    field.isGrouped
                    prop.children [
                        if model.CombineSubmitNext && Option.isSome form.NextButton then
                            Bulma.control.div [
                                Bulma.button.button [
                                    color.isPrimary
                                    prop.disabled model.IsSubmitDisabled
                                    if model.IsSubmitting then button.isLoading
                                    prop.text (form.NextButton |> Option.defaultValue form.SubmitButton)
                                    prop.onClick (fun _ -> dispatch ClickedSubmitAndNext)
                                ]
                            ]
                        else
                            Bulma.control.div [
                                Bulma.button.button [
                                    color.isPrimary
                                    prop.disabled model.IsSubmitDisabled
                                    if model.IsSubmitting then button.isLoading
                                    prop.text form.SubmitButton
                                    prop.onClick (fun _ -> dispatch ClickedSubmit)
                                ]
                            ]
                            if canCancel model then
                                match form.CancelButton with
                                | None -> ()
                                | Some cancel ->
                                    Bulma.control.div [
                                        Bulma.button.button [
                                            prop.text cancel
                                            prop.disabled model.IsSubmitting
                                            prop.onClick (fun _ -> dispatch ClickedCancel)
                                        ]
                                    ]
                            match form.NextButton with
                            | None -> ()
                            | Some next ->
                                Bulma.control.div [ control.isExpanded ]
                                Bulma.control.div [
                                    Bulma.button.button [
                                        prop.text next
                                        prop.disabled model.IsSubmitting
                                        prop.onClick (fun _ -> dispatch ClickedNext)
                                    ]
                                ]
                    ]
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
