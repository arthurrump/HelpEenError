module Index

open Elmish
open Form
open Fable.Remoting.Client
open FsToolkit.ErrorHandling
open Shared.Api
open Shared.Models
open Fable.Core.JS
open System

type Page =
    | Algemeen
    | Demografisch
    | Interview
    | Logboek
    | Bedankt

[<RequireQualifiedAccess>]
type NotificationType =
    | Success
    | Error

type Notification =
    { Guid: Guid
      Message: string
      Type: NotificationType }

[<RequireQualifiedAccess>]
type LogState =
    | Normal
    | Deleting
    | DeletingError of msg: string

type Model =
    { CurrentPage: Page
      ToestemmingForm: ToestemmingForm.Model
      RespondentId: RespondentId option
      RegistrationMoment: DateTime option
      DemografischForm: DemografischForm.Model
      DemografischeGegevens: Demografisch.Result option
      InterviewForm: InterviewForm.Model
      InterviewId: InterviewId option
      LogboekForm: LogboekForm.Model
      Logboek: (LogId * LogState * Logboek.Result) list
      Notifications: Notification list }

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
    | LogboekDeleteEntry of LogId
    | LogboekDeleteEntryResponse of LogId * Result<unit, string>
    | ShowNotification of NotificationType * message: string
    | DismissNotification of Guid
    | ExpireRegistration

let expiration = TimeSpan.FromHours(3)
let isExpired from = (DateTime.Now - from) > expiration
let isSomeExpired = Option.map isExpired >> Option.defaultValue false
let (|IsSomeExpired|) = isSomeExpired
let expireCmd (timespan: TimeSpan) =
    Cmd.ofSub (fun dispatch ->
        printfn "Expiring registration in %f s" timespan.TotalSeconds
        setTimeout (fun () -> dispatch ExpireRegistration) (max 0 (int timespan.TotalMilliseconds)) |> ignore)

let api =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ILogboekApi>

let init (persisted: Model option) : Model * Cmd<Msg> =
    match persisted with
    | None
    | Some { RegistrationMoment = IsSomeExpired true }
    | Some { CurrentPage = Bedankt } ->
        { CurrentPage = Algemeen
          ToestemmingForm = ToestemmingForm.init None
          RespondentId = None
          RegistrationMoment = None
          DemografischForm = DemografischForm.init None
          DemografischeGegevens = None
          InterviewForm = InterviewForm.init None
          InterviewId = None
          LogboekForm = LogboekForm.init None
          Logboek = []
          Notifications = [] }
        , Cmd.none
    | Some p ->
        let cmd =
            match p.RegistrationMoment with
            | Some date ->
                expireCmd (expiration - (DateTime.Now - date))
            | None ->
                Cmd.none
        { CurrentPage = p.CurrentPage
          ToestemmingForm = ToestemmingForm.init (Some p.ToestemmingForm)
          RespondentId = p.RespondentId
          RegistrationMoment = p.RegistrationMoment
          DemografischForm = DemografischForm.init (Some p.DemografischForm)
          DemografischeGegevens = p.DemografischeGegevens
          InterviewForm = InterviewForm.init (Some p.InterviewForm)
          InterviewId = p.InterviewId
          LogboekForm = LogboekForm.init (Some p.LogboekForm)
          Logboek = p.Logboek |> List.map (fun (id, _, log) -> (id, LogState.Normal, log))
          Notifications = [] }
        , cmd

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
            model, Cmd.OfAsync.either api.grantToestemming ()
                (Result.map ToestemmingForm.ToestemmingGranted >> onResponse)
                (fun ex -> onResponse (Error ex.Message))
        | Some respondentId, false ->
            model, Cmd.OfAsync.either api.revokeToestemming (respondentId, model.InterviewId)
                (Result.map (fun _ -> ToestemmingForm.ToestemmingRevoked) >> onResponse)
                (fun ex -> onResponse (Error ex.Message))
        | Some respondentId, true ->
            // We already have given permission, and received an id
            model, Cmd.ofMsg (onResponse (Ok (ToestemmingForm.ToestemmingGranted respondentId)))
        | None, false ->
            model, Cmd.ofMsg (onResponse (Error "Als je niet akkoord gaat, kun je niet meedoen aan het onderzoek."))
    | ToestemmingFormReturn Form.Next ->
        if model.RespondentId |> Option.isSome then
            { model with CurrentPage = Demografisch }, Cmd.none
        else
            { model with CurrentPage = Algemeen }, Cmd.ofMsg (ShowNotification (NotificationType.Error, "Als je niet akkoord gaat, kun je niet meedoen aan het onderzoek."))
    | ToestemmingFormReturn (Form.Submitted (_, response)) ->
        match response with
        | ToestemmingForm.ToestemmingGranted respondentId ->
            { model with
                RespondentId = Some respondentId
                RegistrationMoment = Some DateTime.Now },
            expireCmd expiration
        | ToestemmingForm.ToestemmingRevoked ->
            let model, cmd = init None
            let notif = Cmd.ofMsg (ShowNotification (NotificationType.Success, "Je hebt je toestemming ingetrokken. Alle gegevens zijn verwijderd."))
            { model with CurrentPage = Algemeen }, Cmd.batch [ cmd; notif ]

    | DemografischFormInternal msg ->
        let form, cmd = DemografischForm.update (DemografischFormReturn, DemografischFormInternal) msg model.DemografischForm
        { model with DemografischForm = form }, cmd
    | DemografischFormReturn (Form.Submit (result, onResponse)) ->
        match model.RespondentId with
        | Some respondentId ->
            model, Cmd.OfAsync.either (api.submitDemografisch respondentId) result
                onResponse
                (fun ex -> onResponse (Error ex.Message))
        | None ->
            { model with
                CurrentPage = Algemeen
                ToestemmingForm = ToestemmingForm.init None },
            Cmd.ofMsg (ShowNotification (NotificationType.Error, "Je hebt nog geen toestemming gegeven, of je sessie is verlopen."))
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
            model, Cmd.OfAsync.either (api.submitInterview model.InterviewId) (demografisch, result)
                onResponse
                (fun ex -> onResponse (Error ex.Message))
        | None ->
            { model with
                CurrentPage = Demografisch },
            Cmd.ofMsg (ShowNotification (NotificationType.Error, "Je hebt hier nog geen gegevens ingevuld, of je sessie is verlopen."))
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
            model, Cmd.OfAsync.either (api.submitLog respondentId) result
                onResponse
                (fun ex -> onResponse (Error ex.Message))
        | None ->
            { model with
                CurrentPage = Algemeen
                ToestemmingForm = ToestemmingForm.init None },
            Cmd.ofMsg (ShowNotification (NotificationType.Error, "Je hebt nog geen toestemming gegeven, of je sessie is verlopen."))
    | LogboekFormReturn Form.Next ->
        model, Cmd.none
    | LogboekFormReturn (Form.Submitted (result, logId)) ->
        { model with
            Logboek = (logId, LogState.Normal, result) :: model.Logboek
            LogboekForm = LogboekForm.init None }, Cmd.none
    | LogboekDeleteEntry logId ->
        match model.RespondentId with
        | Some respondentId ->
            { model with
                Logboek =
                    model.Logboek
                    |> List.map (fun (id, state, log) ->
                        if id = logId
                        then (id, LogState.Deleting, log)
                        else (id, state, log)) },
            Cmd.OfAsync.either api.deleteLog (respondentId, logId)
                (fun res -> LogboekDeleteEntryResponse (logId, res))
                (fun ex -> LogboekDeleteEntryResponse (logId, Error ex.Message))
        | None ->
            { model with
                CurrentPage = Algemeen
                ToestemmingForm = ToestemmingForm.init None },
            Cmd.ofMsg (ShowNotification (NotificationType.Error, "Je hebt nog geen toestemming gegeven, of je sessie is verlopen."))
    | LogboekDeleteEntryResponse (logId, result) ->
        match result with
        | Ok () ->
            { model with Logboek = model.Logboek |> List.filter (fun (id, _, _) -> id <> logId) }, Cmd.none
        | Error msg ->
            { model with
                Logboek =
                    model.Logboek
                    |> List.map (fun (id, state, log) ->
                        if id = logId
                        then (id, LogState.DeletingError msg, log)
                        else (id, state, log)) }, Cmd.none

    | ShowNotification (type', msg) ->
        let guid = Guid.NewGuid ()
        let cmd =
            let sub dispatch = setTimeout (fun () -> dispatch (DismissNotification guid)) 10000 |> ignore
            Cmd.ofSub sub
        let notification =
            { Message = msg
              Type = type'
              Guid = guid }
        { model with Notifications = notification :: model.Notifications }, cmd
    | DismissNotification guid ->
        let notifications = List.filter (fun n -> n.Guid <> guid) model.Notifications
        { model with Notifications = notifications }, Cmd.none

    | ExpireRegistration ->
        let model, cmd = init None
        { model with CurrentPage = Bedankt }, cmd

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
            Html.p [
                Html.text "Iedereen maakt fouten. Dat is goed, want van fouten leer je. Maar zoals je misschien wel gemerkt hebt, kan een klein foutje bij "
                Html.text "het programmeren ervoor zorgen dat je helemaal vast loopt. Je krijgt bijvoorbeeld een foutmelding, en kunt pas zien of de rest "
                Html.text "van je programma ook werkt als je die fout hebt opgelost."
            ]
            Html.p [
                Html.text "Normaal gesproken zou je tijdens een les informatica op dat moment je docent om hulp vragen, die dan uitlegd wat er in jouw specifieke "
                Html.text "geval fout gaat en wat je eraan kan doen. Omdat jullie informatica volgen via Co-Teach, is dat helaas niet altijd mogelijk. "
                Html.text "Daarom willen wij onderzoeken hoe we jullie beter kunnen helpen met het oplossen van de fouten die je maakt bij het programmeren, "
                Html.text "ook zonder dat daar een docent bij nodig is."
            ]
            Html.p [
                Html.text "Om dat te doen willen we weten op welke fouten jullie vastlopen. Welke foutmeldingen krijg je waarvan je niet begrijpt wat ze "
                Html.text "betekenen? Wat doet je programma, terwijl je eigenlijk verwacht had dat het iets anders zou doen? Om daar achter te komen vragen we "
                Html.text "jullie om gedurende één les in dit logboek bij te houden op welke fouten jullie vastlopen."
            ]
            Html.p [
                Html.text "Alle gegevens die je invult worden anoniem verwerkt, tenzij bij een vraag anders vermeld is. Na het starten van het logboek "
                Html.text "blijven de gegevens drie uur lang aan jouw computer gekoppeld. Tot die tijd kun je altijd terugkomen naar deze website om aan te "
                Html.text "passen wat je hebt ingevuld, of je toestemming in te trekken en alle gegevens te verwijderen. Dat kun je doen door terug te komen "
                Html.text "naar deze pagina, het vinkje hieronder weer uit te schakelen en op de knop 'Opslaan' te klikken. Na drie uur "
                Html.text "worden de gegevens volledig geanonimiseerd en kunnen ze dus niet meer aan jou gekoppeld worden."
            ]
            Html.p [
                Html.text "Alle gegevens die je invult, worden uitsluitend gebruikt voor het uitvoeren van dit onderzoek. Meedoen is volledig vrijwillig, "
                Html.text "je bent tot niets verplicht. Zoals hiervoor beschreven, kun je tot drie uur na het starten je toestemming intrekken en alle "
                Html.text "ingevulde gegevens verwijderen. Je kunt op elk moment stoppen met het invullen van het logboek. Alle gegevens worden versleuteld "
                Html.text "opgeslagen en alleen gebruikt voor dit onderzoek. Het onderzoek wordt uitgevoerd door Arthur Rump van de faculteit Behavioural, "
                Html.text "Management and Social Sciences op de Universiteit Twente. Als je nog vragen hebt, kun je die stellen via "
                Html.a [ color.hasTextLink; prop.href "mailto:a.h.j.rump@student.utwente.nl"; prop.text "a.h.j.rump@student.utwente.nl" ]
                Html.text "."
            ]
            Html.p [
                Html.text "Als je mee wilt doen aan het onderzoek, en hiermee akkoord gaat, zet dan het vinkje hieronder aan en ga verder naar het logboek."
            ]
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
    Html.div [
        Box.withHeader (dispatch, title = "Logboek", nOutOfN = (4, 4), previousPage = Interview, children = [
            LogboekForm.view (model.LogboekForm) (LogboekFormInternal >> dispatch)
        ])
        Bulma.box [
            if List.isEmpty model.Logboek then
                Html.p [
                    color.hasTextGrey
                    prop.text "Je hebt nog niets aan het logboek toegevoegd"
                ]
            else
                Html.p [
                    spacing.mb5
                    prop.text "Hieronder zie je alle fouten die je aan je logboek hebt toegevoegd. Klik op het kruisje om een item weer te verwijderen."
                ]
            for id, state, log in model.Logboek do
                Bulma.message [
                    Bulma.messageHeader [
                        Bulma.text.p (match log with Logboek.Foutmelding _ -> "Foutmelding" | Logboek.OnverwachtGedrag _ -> "Werkt niet zoals verwacht")
                        if state = LogState.Deleting then
                            Html.span [ prop.className "loader" ]
                        else
                            Html.button [
                                prop.className "delete"
                                prop.onClick (fun _ -> dispatch (LogboekDeleteEntry id))
                            ]
                    ]
                    Bulma.messageBody [
                        match state with
                        | LogState.DeletingError msg ->
                            Bulma.notification [
                                color.isDanger
                                spacing.mb4
                                prop.children [
                                    Html.p $"Sorry, er is iets misgegaan bij het verwijderen: %s{msg}"
                                ]
                            ]
                        | _ -> ()

                        match log with
                        | Logboek.Foutmelding (melding, vervolgactie) ->
                            Bulma.field.div [
                                Bulma.label "Wat is de foutmelding?"
                                Html.pre [ prop.text melding; prop.className "px-3 py-2" ]
                            ]
                            Bulma.field.div [
                                Bulma.label "Wat ga je doen om de fout op te lossen?"
                                Bulma.text.p vervolgactie
                            ]
                        | Logboek.OnverwachtGedrag (beschrijving, verwachting, vervolgactie) ->
                            Bulma.field.div [
                                Bulma.label "Wat verwachtte je dat het programma zou doen?"
                                Bulma.text.p verwachting
                            ]
                            Bulma.field.div [
                                Bulma.label "Wat doet het programma?"
                                Bulma.text.p beschrijving
                            ]
                            Bulma.field.div [
                                Bulma.label "Wat ga je doen om de fout op te lossen?"
                                Bulma.text.p vervolgactie
                            ]
                    ]
                ]
        ]
    ]

let bedankt (model: Model) (dispatch: Msg -> unit) =
    Box.withHeader (dispatch, title = "Bedankt", previousPage = Algemeen, children = [
        Html.p [
            Html.text "Bedankt voor je deelname aan dit onderzoek! Je antwoorden zijn anoniem opgeslagen en dus niet meer aan jou te koppelen. "
            Html.text "Als je nog vragen hebt over het onderzoek, kun je contact opnemen met Arthur Rump via "
            Html.a [ color.hasTextLink; prop.href "mailto:a.h.j.rump@student.utwente.nl"; prop.text "a.h.j.rump@student.utwente.nl" ]
            Html.text ". Als je hebt aangegeven dat je mee wilt doen aan een interview, dan nemen we binnenkort contact met je op."
        ]
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
                            | Bedankt ->
                                bedankt model dispatch

                            if not (List.isEmpty model.Notifications) then
                                Bulma.box [
                                    for notif in model.Notifications do
                                        Bulma.notification [
                                            match notif.Type with
                                            | NotificationType.Success -> color.isSuccess
                                            | NotificationType.Error -> color.isDanger
                                            prop.children [
                                                Html.p notif.Message
                                                Html.button [
                                                    prop.className "delete"
                                                    prop.onClick (fun _ -> dispatch (DismissNotification notif.Guid))
                                                ]
                                            ]
                                        ]
                                ]
                        ]
                    ]
                ]
            ]
        ]
    ]
