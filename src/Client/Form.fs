module FormHelpers

open Feliz
open Feliz.Bulma

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
            value: string,
            update: string -> unit,
            ?title: string,
            ?explainer: seq<Fable.React.ReactElement>,
            ?placeholder: string) =
        Form.titleDiv(?title = title, ?explainer = explainer, children = [
            Bulma.control.div [
                Bulma.input.text [
                    prop.placeholder (defaultArg placeholder "")
                    prop.value value
                    prop.onTextChange update
                ]
            ]
        ])

    static member radio(
            name: string,
            options: (string * string) list,
            selected: string,
            update: string -> unit,
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
                                prop.isChecked ((selected = value))
                                prop.onCheckedChange (fun ch -> if ch then update value)
                                spacing.mr2
                            ]
                            Html.text item
                        ]
                    ]
            ]
        ])

    static member likert(
            name: string,
            value: int,
            update: int -> unit,
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
                            prop.isChecked ((value = i))
                            prop.onCheckedChange (fun ch -> if ch then update i)
                        ]
                ]
            ]
        ])
