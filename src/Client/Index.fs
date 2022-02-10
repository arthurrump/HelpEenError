module Index

open Elmish
open Form
open Fable.Remoting.Client
open FsToolkit.ErrorHandling
open Shared.Api
open Shared.Models

type Page =
    | Algemeen
    | Demografisch
    | Interview
    | Logboek

type Model =
    { CurrentPage: Page
      ToestemmingForm: ToestemmingForm.Model
      RespondentId: RespondentId option
      DemografischForm: DemografischForm.Model
      DemografischeGegevens: Demografisch.Result option
      InterviewForm: InterviewForm.Model
      InterviewId: InterviewId option
      LogboekForm: LogboekForm.Model
      Logboek: (LogId * Logboek.Result) list }

type Msg =
    | GotoPage of Page
    | ToestemmingFormInternal of ToestemmingForm.Msg
    | ToestemmingFormReturn of ToestemmingForm.ReturnMsg<Msg>
    | DemografischFormInternal of DemografischForm.Msg
    | DemografischFormReturn of DemografischForm.ReturnMsg<Msg>
    | InterviewFormInternal of InterviewForm.Msg
    | InterviewFormReturn of InterviewForm.ReturnMsg<Msg>
    | LogboekFormInternal of LogboekForm.Msg
    | LogboekFormReturn of LogboekForm.ReturnMsg<Msg>

let api =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ILogboekApi>

let init () : Model * Cmd<Msg> =
    { CurrentPage = Algemeen
      RespondentId = None
      ToestemmingForm = ToestemmingForm.init ()
      DemografischForm = DemografischForm.init ()
      DemografischeGegevens = None
      InterviewForm = InterviewForm.init ()
      InterviewId = None
      LogboekForm = LogboekForm.init ()
      Logboek = [] }
    , Cmd.none

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | GotoPage page ->
        { model with CurrentPage = page }, Cmd.none

    | ToestemmingFormInternal msg ->
        let form, cmd = ToestemmingForm.update (ToestemmingFormReturn, ToestemmingFormInternal) msg model.ToestemmingForm
        { model with ToestemmingForm = form }, cmd
    | ToestemmingFormReturn (Form.Submit (akkoord, onResponse)) ->
        match model.RespondentId, akkoord with
        | None, true ->
            model, Cmd.OfAsync.result (async {
                let! resp = api.grantToestemming ()
                return onResponse (resp |> Result.map ToestemmingForm.ToestemmingGranted)
            })
        | Some respondentId, false ->
            model, Cmd.OfAsync.result (async {
                let! resp = api.revokeToestemming (respondentId, model.InterviewId)
                return onResponse (resp |> Result.map (fun _ -> ToestemmingForm.ToestemmingRevoked))
            })
        | Some _, true
        | None, false ->
            model, Cmd.none
    | ToestemmingFormReturn Form.Next ->
        if model.RespondentId |> Option.isSome then
            { model with CurrentPage = Demografisch }, Cmd.none
        else
            { model with CurrentPage = Algemeen }, Cmd.none // TODO: Show needs permission page
    | ToestemmingFormReturn (Form.Submitted (_, response)) ->
        match response with
        | ToestemmingForm.ToestemmingGranted respondentId ->
            { model with RespondentId = Some respondentId }, Cmd.none
        | ToestemmingForm.ToestemmingRevoked ->
            let model, cmd = init ()
            { model with CurrentPage = Algemeen }, cmd // TODO: Show revoked page

    | DemografischFormInternal msg ->
        let form, cmd = DemografischForm.update (DemografischFormReturn, DemografischFormInternal) msg model.DemografischForm
        { model with DemografischForm = form }, cmd
    | DemografischFormReturn (Form.Submit (result, onResponse)) ->
        match model.RespondentId with
        | Some respondentId ->
            model, Cmd.OfAsync.result (api.submitDemografisch respondentId result |> Async.map onResponse)
        | None ->
            { model with CurrentPage = Algemeen }, Cmd.none // TODO: Sessie verlopen?
    | DemografischFormReturn Form.Next ->
        { model with CurrentPage = Interview }, Cmd.none
    | DemografischFormReturn (Form.Submitted (result, _)) ->
        { model with DemografischeGegevens = Some result }, Cmd.none

    | InterviewFormInternal msg ->
        let form, cmd = InterviewForm.update (InterviewFormReturn, InterviewFormInternal) msg model.InterviewForm
        { model with InterviewForm = form }, cmd
    | InterviewFormReturn (Form.Submit (result, onResponse)) ->
        match model.DemografischeGegevens with
        | Some demografisch ->
            model, Cmd.OfAsync.result (api.submitInterview model.InterviewId (demografisch, result) |> Async.map onResponse)
        | None ->
            { model with CurrentPage = Demografisch }, Cmd.none // TODO: Some error?
    | InterviewFormReturn Form.Next ->
        { model with CurrentPage = Logboek }, Cmd.none
    | InterviewFormReturn (Form.Submitted (_, interviewId)) ->
        { model with InterviewId = Some interviewId }, Cmd.none

    | LogboekFormInternal msg ->
        let form, cmd = LogboekForm.update (LogboekFormReturn, LogboekFormInternal) msg model.LogboekForm
        { model with LogboekForm = form }, cmd
    | LogboekFormReturn (Form.Submit (result, onResponse)) ->
        match model.RespondentId with
        | Some respondentId ->
            model, Cmd.OfAsync.result (api.submitLog respondentId result |> Async.map onResponse)
        | None ->
            { model with CurrentPage = Demografisch }, Cmd.none // TODO: Sessie verlopen?
    | LogboekFormReturn Form.Next ->
        model, Cmd.none
    | LogboekFormReturn (Form.Submitted (result, logId)) ->
        { model with
            Logboek = (logId, result) :: model.Logboek
            LogboekForm = LogboekForm.init () }, Cmd.none

open Feliz
open Feliz.Bulma

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
        ToestemmingForm.view (model.ToestemmingForm) (ToestemmingFormInternal >> dispatch)
    ])

let demografisch (model: Model) (dispatch: Msg -> unit) =
    Box.withHeader (dispatch, title = "Over jou", nOutOfN = (2, 4), previousPage = Algemeen, children = [
        DemografischForm.view (model.DemografischForm) (DemografischFormInternal >> dispatch)
    ])

let interview (model: Model) (dispatch: Msg -> unit) =
    Box.withHeader (dispatch, title = "Interview?", nOutOfN = (3, 4), previousPage = Demografisch, children = [
        InterviewForm.view (model.InterviewForm) (InterviewFormInternal >> dispatch)
    ])

let logboek (model: Model) (dispatch: Msg -> unit) =
    Box.withHeader (dispatch, title = "Logboek", nOutOfN = (4, 4), previousPage = Interview, children = [
        LogboekForm.view (model.LogboekForm) (LogboekFormInternal >> dispatch)
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
        ]
    ]
