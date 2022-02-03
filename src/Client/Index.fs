module Index

open Elmish
open Fable.Remoting.Client
open Shared.Models

type Page =
    | Algemeen
    | Demografisch
    | Interview
    | Logboek

type Model =
    { CurrentPage: Page
      AlgemeenAkkoord: bool
      DemografischForm: DemografischForm.Model
      InterviewForm: InterviewForm.Model
      LogboekForm: LogboekForm.Model }

type Msg =
    | GotoPage of Page
    | SetAlgemeenAkkoord of bool
    | DemografischUpdated of DemografischForm.Msg
    | DemografischSubmitted of Demografisch.Result
    | InterviewUpdated of InterviewForm.Msg
    | InterviewSubmitted of Interview.Result
    | LogboekUpdated of LogboekForm.Msg
    | LogboekSubmitted of Logboek.Result

// let todosApi =
//     Remoting.createApi ()
//     |> Remoting.withRouteBuilder Route.builder
//     |> Remoting.buildProxy<ITodosApi>

let init () : Model * Cmd<Msg> =
    { CurrentPage = Algemeen
      AlgemeenAkkoord = false
      DemografischForm = DemografischForm.init ()
      InterviewForm = InterviewForm.init ()
      LogboekForm = LogboekForm.init () }
    , Cmd.none

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | GotoPage page ->
        { model with CurrentPage = page }, Cmd.none
    | SetAlgemeenAkkoord akkoord ->
        { model with AlgemeenAkkoord = akkoord }, Cmd.none
    | DemografischUpdated msg ->
        let form, cmd = DemografischForm.update DemografischSubmitted msg model.DemografischForm
        { model with DemografischForm = form }, cmd
    | DemografischSubmitted result ->
        { model with CurrentPage = Interview }, Cmd.none
    | InterviewUpdated msg ->
        let form, cmd = InterviewForm.update InterviewSubmitted msg model.InterviewForm
        { model with InterviewForm = form }, cmd
    | InterviewSubmitted result ->
        { model with CurrentPage = Logboek }, Cmd.none
    | LogboekUpdated msg ->
        let form, cmd = LogboekForm.update LogboekSubmitted msg model.LogboekForm
        { model with LogboekForm = form }, cmd
    | LogboekSubmitted result ->
        { model with LogboekForm = LogboekForm.init () }, Cmd.none

open Feliz
open Feliz.Bulma

let navBrand =
    Bulma.navbarBrand.div [
        Bulma.navbarItem.a [
            navbarItem.isActive
            prop.children [
                Html.i [ prop.className "fas fa-cog" ]
            ]
        ]
    ]

type Box =
    static member withHeader (dispatch, ?title: string, ?nOutOfN, ?previousPage, ?children) =
        let children = defaultArg children Seq.empty
        Bulma.box [
            Html.header [
                spacing.mb6
                prop.style [
                    style.height (length.em 1.2)
                    style.position.relative
                    style.textAlign.center
                ]
                prop.children [
                    match previousPage with
                    | None -> ()
                    | Some previousPage ->
                        Bulma.button.button [
                            color.isLight
                            button.isSmall
                            prop.onClick (fun _ ->
                                dispatch (GotoPage previousPage)
                            )
                            prop.style [
                                style.position.absolute
                                style.top 0
                                style.left 0
                            ]
                            prop.children [
                                Html.i [ prop.className "fas fa-chevron-left mr-2" ]
                                Html.text "Vorige"
                            ]
                        ]
                    match title with
                    | None -> ()
                    | Some title ->
                        Bulma.title [
                            size.isSize4
                            color.hasTextDark
                            prop.text title
                        ]
                    match nOutOfN with
                    | None -> ()
                    | Some (n, outOfN) ->
                        Html.span [
                            color.hasTextGrey
                            prop.style [
                                style.position.absolute
                                style.top 0
                                style.right 0
                            ]
                            prop.textf "%d / %d" n outOfN
                        ]
                ]
            ]
            yield! children
        ]

let algemeenAkkoord (model: Model) (dispatch: Msg -> unit) =
    Box.withHeader (dispatch, title = "Welkom", nOutOfN = (1, 4), children = [
        Bulma.content [
            Bulma.text.p "Amarth aras brith calma caran celeb ech eithel en erin fuin galad gaur glor  hen  idhrin   lalaith lambe lhach  neder nogoth ohtar orna van. Adel aduial aha astaldo avari brethil cabed  ernil esse galen heir heryn hith lambe lenn  nan nar nathron neder nen neth nuquerna raen riel roch thoron. Ampa ast  del glawar gwaith heru hiril  lor mor ranc raw. Ae anor asca caer cennan  en eruanna faroth hith hwesta idhrin lhaw malina naith nim ninn odog paur pethron quesse sigil tad taer tengwa  tri."
            Bulma.text.p "Alqua ambar aran brith craban dol edhel estel heir iaur lambe lenn maeg nathron orne quesse ranc rhun toloth ungol unque. Aina alqua ampa  emerwen hal herves heryn lebed neled nuin peich rhun ros   unque. Aran condir draug esgal lyg moth nuquerna tehta  torech. Ada dor harad him lim naith ninn nogoth orna ost ross thavron thoron. Aha annon ar cam celeb dor ereg galen gannel  grond  laurina luin naneth quesse rhun. Adel ampa bar celeb erin glin gond gwaith hal heledir idhrin lanc lyg  maethor malina mellon  orna parma ril ruin tavor tehta tol toloth."
            Bulma.text.p "Adab alph dagor ereg goth harad haudh heryn  im ithil lanc luin minas naith nen peich per riel rochir rond talan   wen. Aear alata alph annon del dor erin falas fenn  glor hathel  lhach lith  menel mereth min orna ranc thalion. Alata fuin hal hen him lenn nar   tathar thoron tol. Ampa dor gwaith hith iaur laurina nai narn  quesse rhun roch tehta thavron. Adel aha anca anto asta astaldo baran bein canad cor   eneg ereg eruanna falf faun heleg lambe lebed lim neder orna ras sereg sigil tad talagand  wen."
        ]
        Html.form [
            prop.onSubmit (fun ev ->
                ev.preventDefault ()
                dispatch (GotoPage Demografisch)
            )
            prop.children [
                Bulma.field.div [
                    Bulma.input.labels.checkbox [
                        Bulma.input.checkbox [
                            prop.isChecked model.AlgemeenAkkoord
                            prop.onChange (SetAlgemeenAkkoord >> dispatch)
                        ]
                        Bulma.text.span " Ik ga akkoord"
                    ]
                ]
                Bulma.button.button [
                    color.isPrimary
                    prop.disabled (not model.AlgemeenAkkoord)
                    prop.text "Verder"
                ]
            ]
        ]
    ])

let demografisch (model: Model) (dispatch: Msg -> unit) =
    Box.withHeader (dispatch, title = "Over jou", nOutOfN = (2, 4), previousPage = Algemeen, children = [
        DemografischForm.view (model.DemografischForm) (DemografischUpdated >> dispatch)
    ])

let interview (model: Model) (dispatch: Msg -> unit) =
    Box.withHeader (dispatch, title = "Interview?", nOutOfN = (3, 4), previousPage = Demografisch, children = [
        InterviewForm.view (model.InterviewForm) (InterviewUpdated >> dispatch)
    ])

let logboek (model: Model) (dispatch: Msg -> unit) =
    Box.withHeader (dispatch, title = "Logboek", nOutOfN = (4, 4), previousPage = Interview, children = [
        LogboekForm.view (model.LogboekForm) (LogboekUpdated >> dispatch)
    ])

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.hero [
        hero.isFullHeight
        color.isPrimary
        prop.children [
            Bulma.heroBody [
                Bulma.container [
                    Bulma.column [
                        column.is8Desktop
                        column.isOffset2Desktop
                        prop.children [
                            Bulma.title [
                                text.hasTextCentered
                                prop.text "Help, een error!"
                            ]
                            match model.CurrentPage with
                            | Algemeen ->
                                algemeenAkkoord model dispatch
                            | Demografisch ->
                                demografisch model dispatch
                            | Interview ->
                                interview model dispatch
                            | Logboek ->
                                logboek model dispatch
                        ]
                    ]
                ]
            ]
            Bulma.heroFoot [
                Bulma.navbar [
                    Bulma.container [ navBrand ]
                ]
            ]
        ]
    ]
