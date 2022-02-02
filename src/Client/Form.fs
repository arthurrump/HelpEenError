module Form

open Feliz
open Feliz.Bulma

open Shared.Form

let private showError field =
    match field.Error with
    | Some error ->
        [ Bulma.help [ color.isDanger; prop.text error ] ]
    | None ->
        []

type Form =
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
            field: Field<string, 'tres>,
            dispatch: Field.Msg<string> -> unit,
            ?title: string,
            ?explainer: seq<Fable.React.ReactElement>,
            ?placeholder: string) =
        Form.titleDiv(?title = title, ?explainer = explainer, children = [
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

    static member radio(
            name: string,
            options: (string * string) list,
            field: Field<string, 'tres>,
            dispatch: Field.Msg<string> -> unit,
            ?title: string,
            ?explainer: seq<Fable.React.ReactElement>) =
        Form.titleDiv(?title = title, ?explainer = explainer, children = [
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
            field: Field<int, 'tres>,
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
        Form.titleDiv(?title = title, ?explainer = explainer, children = [
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
