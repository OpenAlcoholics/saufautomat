module App

open System
open Card
open Browser
open Elmish
open Elmish.React
open Fable.Import
open Fable.React
open Fable.React.Props
open Helper
open Model
open Player
open Resources
open System.Text.RegularExpressions
open Thoth.Fetch
open View

let getCards language (version: CardsVersion) dispatch =
    promise {
        let url =
            match version with
            | CardsVersion.I18N ->
                (sprintf
                    "https://raw.githubusercontent.com/OpenAlcoholics/drinking-game-cards/feature/i18n/minified_%s.json"
                     language)
            | CardsVersion.V2 -> "https://raw.githubusercontent.com/OpenAlcoholics/drinking-game-cards/v2/minified.json"

        let! res = Fetch.get (url)
        AddCards res |> dispatch
    }

let init (): Model * Cmd<Msg> =
    { Players = List.empty
      ActiveCards = List.empty
      CurrentCard = None
      Cards = List.empty
      CurrentPlayer = None
      Counter = 0
      DisplayPlayerNameDuplicateError = false
      InitialLoad = true
      Settings =
          { MinimumSips = int (unwrapOr (findCookieValue "minimum-sips") "2")
            MaximumSips =
                (unwrapOr (findCookieValue "maximum-sips") "10")
                |> int
            Remote = (unwrapOr (findCookieValue "remote") "true") = "true"
            Audio = (unwrapOr (findCookieValue "audio") "true") = "true"
            Language =
                (unwrapOr (findCookieValue "language") (Seq.head ((unwrapOr navigator.language "en-US").Split '-')))
            CardsVersion = stocv (unwrapOr (findCookieValue "cardsversion") "v2") }
      Round = 0
      RoundInformation =
          { CardsToPlay = 0
            InitialPlayerIndex = -1 } },
    Cmd.Empty

let rec findNextActivePlayer (playerList: Player.Type list) model =
    if playerList.Length = 0 then
        None
    else if playerList.Length = 1 then
        Some playerList.Head
    else
        match model.CurrentPlayer with
        | Some player ->
            match List.tryFindIndex ((=) player) playerList with
            | Some index ->
                let player =
                    playerList.Item((index + 1) % playerList.Length)

                match player.Active with
                | false -> findNextActivePlayer (List.filter ((<>) player) playerList) model
                | true -> Some player
            | None -> Some model.Players.Head
        | None -> Some model.Players.Head

let int_replacement_regex = Regex("{int([^}]*)?}")

let filterCardsForTurn cards model =
    let distinctCount = (getDistinctCount cards)

    let cards =
        List.filter (fun card ->
            card.Count > 0
            && ((model.Settings.Remote && card.Remote)
                || (not model.Settings.Remote))
            && if model.Players.Length = 0 then (not card.Personal) && card.Rounds = 0 else true) cards

    let cards =
        List.filter (fun card ->
            if card.Unique && not card.Personal then
                (List.filter (fun (activeCard, _) -> card.Unique && (activeCard = card)) model.ActiveCards)
                    .Length = 0
            else
                true) cards

    List.filter (fun card ->
        if distinctCount > 1 then
            card.Id
            <> (unwrapMapOrDefault model.CurrentCard (fun c -> c.Id) -1)
        else
            true) cards

let rec randomExclusive min max (unusable: int list) =
    let r =
        (Random().Next()) % (max - min + 1) + min

    let forceReturn =
        (max - min + 1) <= (List.length unusable)

    if List.exists ((=) r) unusable && not forceReturn
    then randomExclusive min max unusable
    else r

let replaceCardText card model =
    let min = model.Settings.MinimumSips
    let max = model.Settings.MaximumSips

    let text =
        Regex.Replace(card.Text, "{int[^}]*}", "{int}")

    let mutable unusable = List.Empty

    let replacement_text =
        (Seq.map (fun (w: string) ->
            // This is temporary until the parser and variable replacement is implemented
            let m = int_replacement_regex.Match w

            match m.Success with
            | true ->
                let r = randomExclusive min max unusable
                unusable <- r :: unusable
                (sprintf "%d" r)
            | false -> w) (text.Split ' '))
        |> String.concat " "

    { card with Text = replacement_text }

let getNextCard cards model =
    let cards = filterCardsForTurn cards model

    if cards.Length = 0
    then None
    else Some(cards.Item(Random().Next() % cards.Length))

let explodeCards cards =
    (List.map (fun card ->
        ([ card ]
         |> Seq.collect (fun c -> List.replicate c.Count { c with Count = 1 }))) cards)
    |> Seq.reduce Seq.append
    |> List.ofSeq

let roundHasEnded model =
    model.RoundInformation.CardsToPlay <= 0
    && model.Players.Length > 0

let getPlayerByIndex index (players: Player.Type list): Player.Type option =
    try
        Some(players.Item index)
    with _ -> None

let allowedLanguages = [ "de"; "en" ]

let generateActiveCardId card player isModal isId =
    sprintf
        "%sactivecardnote%s%d%s"
        (if isId then "#" else "")
        (if isModal then "modal" else "")
        card.Id
        (unwrapMapOrDefault player (fun p -> p.GId) "")

let update (msg: Msg) (model: Model) =
    match msg with
    | InitialLoad ->
        { model with InitialLoad = false },
        Cmd.ofSub (fun dispatch ->
            getCards model.Settings.Language model.Settings.CardsVersion dispatch
            |> Promise.start)
    | AdvanceTurn ->
        model,
        Cmd.ofSub (fun dispatch ->
            do dispatch PlayAudio
               dispatch IncrementCounter
               dispatch ChangeActivePlayer
               dispatch ChangeActiveCard
               dispatch DecrementPlayerRoundCards

               if roundHasEnded model then
                   dispatch AdvanceRound
                   dispatch DecrementActiveRoundCards)
    | PlayAudio ->
        if model.Settings.Audio then
            let audioId =
                (if roundHasEnded model then "nextround-audio" else "nextcard-audio")

            stop "nextcard-audio"
            stop "nextround-audio"
            play audioId

        model, Cmd.Empty
    | ChangeActiveCard ->
        let playerCards =
            filterActiveCards (model.CurrentPlayer.Value) (model.ActiveCards)

        let cards =
            if model.CurrentPlayer.IsSome
            then List.filter (fun card -> not (List.exists (fun c -> c = card) playerCards)) model.Cards
            else model.Cards

        let card = getNextCard cards model

        let model =
            { model with
                  CurrentCard = card
                  Cards = decreaseCount card model.Cards
                  RoundInformation =
                      { model.RoundInformation with
                            CardsToPlay = max (model.RoundInformation.CardsToPlay - 1) 0 } }

        model,
        (if card.IsSome then
            Cmd.ofSub (fun dispatch ->
                (if (isActiveCard card) then
                    AddActiveCard(card.Value, (if card.Value.Personal then model.CurrentPlayer else None))
                    |> dispatch))
         else
             Cmd.Empty)
    | ChangeActivePlayer ->
        let nextPlayer =
            findNextActivePlayer
                ((List.filter (fun p ->
                    p.Active
                    || (compareOption (model.CurrentPlayer) (Some p))) model.Players))
                model

        (if model.Counter <> 0 then
            { model with
                  CurrentPlayer = nextPlayer
                  Players =
                      List.map (fun player ->
                          if (compareOption (Some player) nextPlayer) then
                              { player with
                                    CardsPlayed = player.CardsPlayed + 1 }
                          else
                              player) model.Players }
         else
             model),
        Cmd.Empty
    | IncrementCounter ->
        { model with
              Counter = model.Counter + 1 },
        Cmd.Empty
    | AddCards cards ->
        { model with
              Cards = explodeCards (List.map Into cards) },
        Cmd.Empty
    | AddPlayer player ->
        { model with
              Players = model.Players @ [ player ]
              RoundInformation =
                  { model.RoundInformation with
                        CardsToPlay = model.RoundInformation.CardsToPlay + 1 } },
        match model.CurrentPlayer with
        | Some _ -> Cmd.Empty
        | None -> Cmd.ofSub (fun dispatch -> dispatch ChangeActivePlayer)
    | RemovePlayer player ->
        let isCurrentPlayer = isCurrent player model.CurrentPlayer

        let players =
            (List.filter (fun p -> p <> player) model.Players)

        let activeCards =
            List.filter (fun (_, p) -> not (compareOption p (Some player))) model.ActiveCards

        let playerIndex =
            unwrapOr (getIndex (Some player) model.Players) 0

        let currentPlayerIndex =
            unwrapOr (getIndex model.CurrentPlayer model.Players) 0

        let cardsToPlayModifier =
            match (playerIndex > model.RoundInformation.InitialPlayerIndex
                   && playerIndex < model.Players.Length
                   && playerIndex < currentPlayerIndex)
                  || (playerIndex < model.RoundInformation.InitialPlayerIndex
                      && playerIndex < currentPlayerIndex) with
            | true -> 0
            | false -> -1

        { model with
              Players = players
              ActiveCards = activeCards
              RoundInformation =
                  { model.RoundInformation with
                        CardsToPlay =
                            model.RoundInformation.CardsToPlay
                            + cardsToPlayModifier } },
        (if isCurrentPlayer
         then Cmd.ofSub (fun dispatch -> dispatch AdvanceTurn)
         else Cmd.Empty)
    | TogglePlayerActivity player ->
        { model with
              Players = (List.map (fun p -> if p = player then { p with Active = not p.Active } else p) model.Players) },
        (if (isCurrent player model.CurrentPlayer)
         then Cmd.ofSub (fun dispatch -> dispatch AdvanceTurn)
         else Cmd.Empty)
    | DisplayPlayerNameDuplicate ->
        { model with
              DisplayPlayerNameDuplicateError = true },
        Cmd.Empty
    | HidePlayerNameDuplicate ->
        { model with
              DisplayPlayerNameDuplicateError = false },
        Cmd.Empty
    | AddActiveCard (card, player) ->
        let player = if card.Personal then player else None

        { model with
              ActiveCards =
                  ({ card with
                         StartingRound = if card.Rounds > 0 then Some model.Round else None },
                   player)
                  :: model.ActiveCards },
        Cmd.Empty
    | DecrementActiveRoundCards ->
        { model with
              ActiveCards =
                  (List.filter (fun (card, _) ->
                      (card.Rounds <> 0 && card.Uses = 0)
                      || card.Uses <> 0)
                       (List.map (fun (card, player: Player.Type option) ->
                           { card with
                                 Rounds =
                                     if card.Rounds > 0 && card.Uses = 0 && player.IsNone
                                     then card.Rounds - 1
                                     else card.Rounds },
                           player) model.ActiveCards)) },
        Cmd.Empty
    | DecrementPlayerRoundCards ->
        { model with
              ActiveCards =
                  List.filter (fun (c, _) -> (c.Rounds <> 0 || c.Uses > 0))
                      (List.map (fun (card, player) ->
                          (if compareOption model.CurrentPlayer player then
                              { card with
                                    Rounds =
                                        if card.Rounds > 0 && card.Uses = 0 && player.IsSome
                                        then card.Rounds - 1
                                        else card.Rounds }
                           else
                               card),
                          player) model.ActiveCards) },
        Cmd.Empty
    | UseActiveCard (card, player) ->
        { model with
              ActiveCards =
                  List.filter (fun (c, p) ->
                      (c.Rounds <> 0 || c.Uses > 0)
                      && not
                          (card.Uses = 1
                           && card = c
                           && (compareOption (Some player) p)))
                      (List.map (fun (c, p) ->
                          (if card = c && (compareOption (Some player) p) then
                              { c with
                                    Uses = c.Uses - 1
                                    StartingRound = Some model.Round }
                           else
                               c),
                          p) model.ActiveCards) },
        Cmd.ofSub (fun dispatch ->
            AddActiveCard({ card with Uses = 0 }, (Some player))
            |> dispatch)
    | ChangeRemoteSetting ->
        let remote = getCheckedFromHtmlElement "remote"

        JsCookie.set "remote" (sprintf "%b" remote)
        |> ignore

        { model with
              Settings = { model.Settings with Remote = remote } },
        Cmd.Empty
    | ChangeAudioSetting ->
        let audio = getCheckedFromHtmlElement "audio"

        JsCookie.set "audio" (sprintf "%b" audio)
        |> ignore

        { model with
              Settings = { model.Settings with Audio = audio } },
        Cmd.Empty
    | SaveSettings ->
        let min =
            (match (getValueFromHtmlInput "minimum-sips" "") with
             | "" -> model.Settings.MinimumSips
             | value -> value |> int)

        JsCookie.set "minimum-sips" (sprintf "%d" min)
        |> ignore

        let max =
            (match (getValueFromHtmlInput "maximum-sips" "") with
             | "" -> model.Settings.MaximumSips
             | value -> value |> int)

        JsCookie.set "maximum-sips" (sprintf "%d" max)
        |> ignore

        let language =
            (match (getValueFromHtmlInput "language" "") with
             | "" -> model.Settings.Language
             | value -> value)

        let language =
            if not (List.exists ((=) language) allowedLanguages)
            then model.Settings.Language
            else language

        JsCookie.set "language" language |> ignore

        let cardsversion =
            (match (getValueFromHtmlInput "cardsversion" "") with
             | "" -> model.Settings.CardsVersion
             | value -> stocv value)

        JsCookie.set "cardsversion" (cvtos cardsversion)
        |> ignore

        { model with
              Settings =
                  { model.Settings with
                        MinimumSips = min
                        MaximumSips = max } },
        Cmd.ofSub (fun dispatch ->
            do (if (language <> model.Settings.Language
                    && cardsversion <> CardsVersion.V2) then
                    ChangeLanguage language
                else
                    NoopMsg)
               |> dispatch

               (if cardsversion <> model.Settings.CardsVersion
                then ChangeCardsVersion cardsversion
                else NoopMsg)
               |> dispatch)
    | Reset -> init ()
    | AdvanceRound ->
        (if model.Players.Length > 0 then
            { model with
                  Round = model.Round + 1
                  RoundInformation =
                      { CardsToPlay = (getActive model.Players).Length - 1
                        InitialPlayerIndex =
                            (unwrapOr (getIndex model.CurrentPlayer model.Players) 1)
                            - 1 } }
         else
             model),
        Cmd.Empty
    | RemoveActiveCard card ->
        { model with
              ActiveCards = List.filter (fun (c, _) -> card <> c) model.ActiveCards },
        Cmd.Empty
    | RemoveCardFromSession card ->
        { model with
              Cards = List.filter (fun c -> card <> c) model.Cards },
        Cmd.ofSub (fun dispatch -> dispatch AdvanceTurn)
    | ChangeLanguage language ->
        ((Dom.window.document.getElementsByTagName "html").Item 0)
            .setAttribute("lang", language)

        { model with
              Settings =
                  { model.Settings with
                        Language = language }
              ActiveCards = [] },
        Cmd.ofSub (fun dispatch ->
            getCards language model.Settings.CardsVersion dispatch
            |> Promise.start)
    | ChangeCardsVersion version ->
        { model with
              Settings =
                  { model.Settings with
                        CardsVersion = version }
              ActiveCards = [] },
        Cmd.ofSub (fun dispatch ->
            getCards model.Settings.Language version dispatch
            |> Promise.start)
    | AddNoteToActiveCard (card, player) ->
        let note =
            match (getValueFromHtmlInput (generateActiveCardId card player false false) "") with
            | "" -> None
            | value -> Some value

        { model with
              ActiveCards =
                  List.map (fun (c, player) -> (if card = c then { card with Note = note } else c), player)
                      model.ActiveCards },
        Cmd.Empty
    | ReassignCard card ->
        match (getValueFromHtmlInput "reassignplayeroption" "") with
        | "" -> model, Cmd.Empty
        | name ->
            match List.tryFind (fun p -> p.Name = name) model.Players with
            | Some newPlayer ->
                let activeCards =
                    List.map (fun (c, player) -> c, (if card = c then Some newPlayer else player)) model.ActiveCards

                { model with ActiveCards = activeCards }, Cmd.Empty
            | None -> model, Cmd.Empty
    | NoopMsg -> model, Cmd.Empty
    | SendReview ->
        let id =
            getValueFromHtmlInput "card-review-id" ""

        let text =
            getValueFromHtmlInput "card-review-text" ""

        let count =
            getValueFromHtmlInput "card-review-count" ""

        let uses =
            getValueFromHtmlInput "card-review-uses" ""

        let rounds =
            getValueFromHtmlInput "card-review-rounds" ""

        let personal =
            getValueFromHtmlInput "card-review-personal" ""

        let remote =
            getValueFromHtmlInput "card-review-remote" ""

        let unique =
            getValueFromHtmlInput "card-review-unique" ""

        let note =
            getValueFromHtmlInput "card-review-note" ""

        // TODO: wait until the backend is ready

        model, Cmd.Empty

let settings model dispatch =
    let body =
        [ div [ ClassName "row" ] [
            label "minimum-sips" model "SETTINGS_MINIMUM_SIPS"
            input "minium-sips" "minium-sips" "text" "" (sprintf "%d" (model.Settings.MinimumSips)) "\d{1,2}" None []
          ]
          div [ ClassName "row" ] [
              label "maximum-sips" model "SETTINGS_MAXIMUM_SIPS"
              input "maximum-sips" "maximum-sips" "text" "" (sprintf "%d" (model.Settings.MaximumSips)) "\d{1,2}" None []
          ]
          div [ ClassName "row" ] [
              label "remote" model "SETTINGS_REMOTE"
              input "remote" "remote" "remote" "" "" "" None  [ OnClick(fun _ -> dispatch ChangeRemoteSetting)
                                                                DefaultChecked(model.Settings.Remote) ]
          ]
          div [ ClassName "row" ] [
              label "audio" model "SETTINGS_AUDIO"
              input "audio" "audio" "checkbox" "" "" "" None  [ OnClick(fun _ -> dispatch ChangeAudioSetting)
                                                                DefaultChecked(model.Settings.Audio) ]

          ]
          div [ ClassName "row" ] [
              label "language" model "SETTINGS_LANGUAGE"
              select
                  [ Name "language"
                    ClassName "m-1 w-100 col"
                    Id "language"
                    Disabled(model.Settings.CardsVersion = CardsVersion.V2)
                    DefaultValue model.Settings.Language ]
                  (if model.Settings.CardsVersion = CardsVersion.V2
                   then [ option [] [ str "de" ] ]
                   else (List.map (fun language -> option [] [ str language ]) allowedLanguages))
          ]
          div [ ClassName "row" ] [
              label "cardsversion" model "SETTINGS_CARDSVERSION"
              select [ Name "cardsversion"
                       ClassName "m-1 w-100 col"
                       Id "cardsversion"
                       DefaultValue model.Settings.Language ] [
                  option [] [
                      str (getKey (model.Settings.Language) "SETTINGS_CARDSVERSION_OPTION_V2")
                  ]
                  option [] [
                      str (getKey (model.Settings.Language) "SETTINGS_CARDSVERSION_OPTION_I18N")
                  ]
              ]
          ] ]

    let footer =
        [ span [ Id "git-tag"
                 ClassName "text-secondary {{TAG-CLASS}}" ] [
            str "{{TAG}}"
          ]
          button [ ClassName "btn btn-primary"
                   DataDismiss "modal"
                   OnClick(fun _ -> dispatch SaveSettings) ] [
              str (getKey (model.Settings.Language) "SETTINGS_SAVE")
          ] ]

    modal "settings" body footer

let review card model dispatch =
    let body =
        [ div [ ClassName "row" ] [
            label "card-review-id" model "ID"
            input "card-review-id" "card-review-id" "text" (sprintf "%d" card.Id) (sprintf "%d" card.Id) "-?\d+" None []
          ]
          div [ ClassName "row" ] [
              label "card-review" model "CARD_REVIEW_TEXT"
              input "card-review-text" "card-review-text" "text" card.Text "" "" None []
          ]
          div [ ClassName "row" ] [
              label "card-review-count" model "CARD_REVIEW_COUNT"
              input "card-review-count" "card-review-count" "text" (sprintf "%d" (card.Count)) (sprintf "%d" (card.Count)) "\d+" None []
          ]
          div [ ClassName "row" ] [
              label "card-review-uses" model "CARD_REVIEW_USES"
              input "card-review-uses" "card-review-uses" "text" (sprintf "%d" (card.Uses)) (sprintf "%d" (card.Uses)) "" None []
          ]
          div [ ClassName "row" ] [
              label "card-review-rounds" model "CARD_REVIEW_ROUNDS"
              input "card-review-rounds" "card-review-rounds" "text" (sprintf "%d" (card.Rounds)) (sprintf "%d" (card.Rounds)) "\d+" None []
          ]
          div [ ClassName "row" ] [
              label "card-review-personal" model "CARD_REVIEW_PERSONAL"
              select [ Name "card-review-personal"
                       ClassName "m-1 w-100 col"
                       Id "card-review-personal"
                       DefaultValue(card.Personal.ToString()) ] [
                  option [] [ str "true" ]
                  option [] [ str "false" ]
              ]
          ]
          div [ ClassName "row" ] [
              label "card-review-remote" model "CARD_REVIEW_REMOTE"
              select [ Name "card-review-remote"
                       ClassName "m-1 w-100 col"
                       Id "card-review-remote"
                       DefaultValue(card.Remote.ToString()) ] [
                  option [] [ str "true" ]
                  option [] [ str "false" ]
              ]
          ]
          div [ ClassName "row" ] [
              label "card-review-unique" model "CARD_REVIEW_UNIQUE"
              select [ Name "card-review-unique"
                       ClassName "m-1 w-100 col"
                       Id "card-review-unique"
                       DefaultValue(card.Unique.ToString()) ] [
                  option [] [ str "true" ]
                  option [] [ str "false" ]
              ]
          ]
          hr []
          div [ ClassName "row" ] [
              label "card-review-note" model "CARD_REVIEW_NOTE"
              textarea [ Name "card-review-note"
                         ClassName "m-1 w-100 col"
                         Id "card-review-note"
                         InputType "textarea" ] []
          ] ]

    let footer =
        [ button [ ClassName "btn btn-primary"
                   DataDismiss "modal"
                   OnClick(fun _ -> dispatch SendReview) ] [
            str (getKey (model.Settings.Language) "CARD_REVIEW_SAVE")
          ] ]

    modal "card-review" body footer

let addPlayer name model dispatch =
    match List.tryFind ((=) (create name)) model.Players with
    | Some _ -> false
    | None ->
        dispatch (AddPlayer(create name))
        HidePlayerNameDuplicate |> ignore
        true

let addPlayerFunction model dispatch =
    match getValueFromHtmlInput "add-player-field" "" with
    | "" -> ()
    | value ->
        (match (addPlayer value model dispatch) with
         | true ->
             ((Dom.window.document.getElementById "add-player-field") :?> Browser.Types.HTMLInputElement).value <- ""
             HidePlayerNameDuplicate |> dispatch
         | false -> DisplayPlayerNameDuplicate |> dispatch)

let displayPlayer player model dispatch =
    div [ ClassName
              ("flex-column mr-2 card "
               + (if model.CurrentPlayer.IsSome
                     && model.CurrentPlayer.Value.Name = player.Name then
                   "bg-primary"
                  else if not player.Active then
                      "border-warning"
                  else
                      "")) ] [
        div [ ClassName "card-body" ] [
            h5 [ ClassName "card-title text-center" ] [
                str (sprintf "%s (%d)" player.Name player.CardsPlayed)
            ]
            div [ ClassName "d-flex justify-content-between" ] [
                button [ ClassName "card btn btn-secondary toggle-button"
                         OnClick(fun _ -> TogglePlayerActivity player |> dispatch) ] [
                    str
                        (if player.Active
                         then (getKey (model.Settings.Language) "PLAYER_SUSPEND_ON")
                         else (getKey (model.Settings.Language) "PLAYER_SUSPEND_OFF"))
                ]
                button [ ClassName "card btn btn-secondary delete-button"
                         OnClick(fun _ -> RemovePlayer player |> dispatch) ] [
                    str (getKey (model.Settings.Language) "PLAYER_DELETE")
                ]
            ]
        ]
    ]

let sidebar (model: Model) dispatch =
    div [ ClassName "col-lg-2 sidebar col h-100 d-none d-lg-block d-xl-block" ] [
        div [ ClassName "form-group" ] [
            input "add-player-field" "add-player-field" "form-control m-1 w-100" "" "" "" None [ OnKeyDown(fun x -> if x.keyCode = 13. then (addPlayerFunction model dispatch))
                                                                                                 MaxLength 20. ]
            (if model.DisplayPlayerNameDuplicateError then
                div [ ClassName "alert alert-danger ml-1" ] [
                    str (getKey (model.Settings.Language) "DUPLICATE_PLAYER_ERROR")
                ]
             else
                 span [] [ str "" ])
            button [ ClassName "btn btn-primary m-1 w-100"
                     OnClick(fun _ -> addPlayerFunction model dispatch) ] [
                str (getKey (model.Settings.Language) "ADD_PLAYER")
            ]
        ]
        hr []
        div
            [ ClassName "flex-row mb-4"
              Style [ OverflowY "scroll"
                      Height "85%" ] ]
            (List.map (fun p -> displayPlayer p model dispatch)
                 (model.Players
                  |> Seq.sortBy (fun p -> not p.Active)
                  |> List.ofSeq))
    ]

let displayCurrentCard model dispatch =
    div [ ClassName "card d-flex col"
          Id "active-card" ] [
        (match model.CurrentCard with
         | Some (card) -> (review card model dispatch)
         | None -> span [] [])
        div [ ClassName "card-body flex-wrap" ] [
            button [ OnClick(fun _ -> dispatch AdvanceTurn)
                     ClassName "card-body card-title btn btn-dark w-100"
                     Style [ Height "93%" ]
                     Id "current-card-body"
                     Disabled(model.CurrentCard.IsNone && model.Counter > 0) ] [
                span [ ClassName "h3" ] [
                    str
                        (match model.CurrentCard with
                         | Some (card) -> (replaceCardText card model).Text
                         | None ->
                             if model.Counter = 0
                             then (getKey (model.Settings.Language) "CLICK_TO_START")
                             else (getKey (model.Settings.Language) "NO_CARDS_LEFT"))
                ]
            ]
            div [ ClassName "row" ] [
                button [ ClassName "btn btn-secondary d-none d-md-block d-lg-block d-xl-block mr-2"
                         Disabled model.CurrentCard.IsNone
                         OnClick(fun _ ->
                             if model.CurrentCard.IsSome then
                                 RemoveCardFromSession model.CurrentCard.Value
                                 |> dispatch) ] [
                    str (getKey (model.Settings.Language) "DELETE_CARD_FROM_SESSION")
                ]
                modalButton "card-review" "btn btn-secondary d-none d-md-block d-lg-block d-xl-block" model.CurrentCard.IsNone model "CARD_REVIEW" []
                (if model.CurrentCard.IsSome
                    && model.CurrentCard.Value.Personal then
                    span [ ClassName "badge badge-secondary m-2"
                           Style [ FontSize "0.9rem" ] ] [
                        str (getKey (model.Settings.Language) "PERSONAL_CARD_INDICATOR")
                    ]
                 else
                     span [] [])
            ]
        ]
    ]

let displayInformationHeader model =
    let separator: ReactElement = span [] [ str " | " ]

    let elements =
        [ (span [] [
            str
                (match model.CurrentPlayer with
                 | Some player -> player.Name
                 | None -> (getKey (model.Settings.Language) "NO_ACTIVE_PLAYER"))
           ])
          (span [ Title(getKey (model.Settings.Language) "NUMBER_CARDS_PLAYED") ] [
              str (sprintf "%s %d" (getKey (model.Settings.Language) "CARDS_PLAYED") model.Counter)
           ])
          (span [] [
              str (sprintf "%s %d" (getKey (model.Settings.Language) "ROUND") model.Round)
           ]) ]

    div
        [ Id "active-player-header"
          ClassName "text-center col text-truncate h3 d-none d-lg-block d-xl-block" ]
        (joinHtmlElements separator elements)

let addNoteToActiveCardModal card player model dispatch =
    div [ ClassName "modal fade"
          Id(generateActiveCardId card player true false)
          TabIndex -1
          Role "dialog" ] [
        div [ ClassName "modal-dialog"
              Role "document" ] [
            div [ ClassName "modal-content" ] [
                div [ ClassName "modal-body" ] [
                    div [ ClassName "form-group container" ] [
                        div [ ClassName "row" ] [
                            input "note" (generateActiveCardId card player false false) "text" "" (unwrapOr card.Note "Enter a note here...") "" None []
                        ]
                    ]
                ]
                div [ ClassName "modal-footer" ] [
                    button [ ClassName "btn btn-primary"
                             DataDismiss "modal"
                             OnClick(fun _ -> AddNoteToActiveCard(card, player) |> dispatch) ] [
                        str (getKey (model.Settings.Language) "ACTIVE_CARD_SAVE_NOTE")
                    ]
                ]
            ]
        ]
    ]

let playerSelection players =
    select
        [ Name "player"
          ClassName "m-1 w-100 col"
          Id "reassignplayeroption" ]
        (List.map (fun player -> option [] [ str player.Name ]) players)

let playerListModal model dispatch card (player: Player.Type option) =
    let players =
        List.filter ((<>) player.Value) model.Players

    let body =
        [ div [ ClassName "row" ] [
            playerSelection players
          ] ]

    let footer =
        [ button [ ClassName "btn btn-primary"
                   DataDismiss "modal"
                   OnClick(fun _ -> ReassignCard card |> dispatch) ] [
            str (getKey (model.Settings.Language) "ACTIVE_CARD_SAVE_NOTE")
          ] ]

    modal "playerlistmodal" body footer

let displayActiveCard (card, player: Player.Type option) model dispatch =
    div [ ClassName
              ("card p-2 m-1"
               + (if compareOption player model.CurrentPlayer then " border-success"
                  else if player.IsNone then " border-warning"
                  else ""))
          Style [ Height "1%" ]
          Title card.Text ] [
        (addNoteToActiveCardModal card player model dispatch)
        h5 [ ClassName "card-title h-50" ] [
            str
                ((if player.IsSome then (sprintf "[%s] " player.Value.Name) else "")
                 + card.Text
                 + (if card.Rounds > 0 && card.Uses = 0 then (sprintf " (%d)" card.Rounds) else ""))
        ]
        div [ ClassName "card-body text-center mb-2" ] [
            (match card.Note with
             | Some value -> h6 [] [ em [] [ str value ] ]
             | None -> span [] [])
            (match card.StartingRound with
             | Some value ->
                 h6 [] [
                     em [] [
                         str (sprintf "Started in round: %d" value)
                     ]
                 ]
             | None -> span [] [])
            (if card.Uses > 0 && player.IsSome then
                button [ ClassName "btn btn-primary mb-1 w-100"
                         OnClick(fun _ -> UseActiveCard(card, player.Value) |> dispatch) ] [
                    str (sprintf "%s (%d)" (getKey (model.Settings.Language) "ACTIVE_CARD_USE") card.Uses)
                ]
             else
                 span [] [])
            button [ ClassName "btn btn-primary mr-1"
                     Style [ Width "49%" ]
                     OnClick(fun _ -> RemoveActiveCard card |> dispatch) ] [
                str (getKey (model.Settings.Language) "ACTIVE_CARD_DELETE")
            ]
            button [ ClassName "btn btn-primary"
                     Style [ Width "49%" ]
                     DataToggle "modal"
                     DataTarget(generateActiveCardId card player true true) ] [
                str (getKey (model.Settings.Language) "ACTIVE_CARD_ADD_NOTE")
            ]
            if player.IsSome && model.Players.Length > 1 then
                playerListModal model dispatch card player

                modalButton "playerlistmodal" "btn btn-secondary d-none d-md-block d-lg-block d-xl-block" (model.Players.Length > 1) model "ACTIVE_CARD_REASSIGN" [ Style [ Width "49%"; MarginTop "1%" ] ]
        ]
    ]

let activeCards (model: Model) dispatch =
    div [ ClassName "active-cards row mt-3 d-none d-lg-block d-xl-block"
          Style [ Height "22%"
                  OverflowY "scroll"
                  MarginRight "1.67em" ] ] [
        div
            [ ClassName "col d-flex flex-wrap" ]
            (List.map (fun card -> displayActiveCard card model dispatch)
                 (List.rev (List.sortBy (fun (_, player) -> compareOption model.CurrentPlayer player) model.ActiveCards)))
    ]

let view (model: Model) dispatch =
    div [ Ref(fun element ->
              if not (isNull element)
              then if model.InitialLoad then dispatch InitialLoad)
          ClassName "container-fluid h-100" ] [
        div [ ClassName "row m-4" ] [
            figure [] [
                audio [ Id "nextcard-audio"
                        Src "/nextcard.mp3" ] []
            ]
            figure [] [
                audio [ Id "nextround-audio"
                        Src "/nextround.mp3" ] []
            ]
            span [ Id "cardToReassign" ] []
            div [ ClassName "col-sm-8 col-lg-2" ] [
                div [ ClassName "" ] [
                    modalButton "settings" "btn btn-primary m-1" false model "SETTINGS" []
                    button [ ClassName "btn btn-primary m-1"
                             OnClick(fun _ -> dispatch Reset) ] [
                        str (getKey (model.Settings.Language) "RESET")
                    ]
                    (match navigator.userAgent.Contains "Android" with
                     | true ->
                         a [ ClassName "d-sm-block d-lg-none m-1"
                             Href "https://play.google.com/store/apps/details?id=group.openalcoholics.sam" ] [
                             img [ Alt(getKey (model.Settings.Language) "GOOGLE_PLAY_IMG_ALT")
                                   Src(sprintf "google-play-badge_%s.png" model.Settings.Language)
                                   Style [ Height "40%"; Width "40%" ] ]
                         ]
                     | false -> span [] [])
                ]
            ]
            displayInformationHeader model
            span [ ClassName "text-secondary col-2" ] [
                str (sprintf "%s: " (getKey (model.Settings.Language) "CONTACT"))
                a [ Id "contactlink"
                    Href "mailto:saufautomat@carstens.tech"
                    ClassName "text-secondary" ] [
                    str "saufautomat@carstens.tech"
                ]
            ]
        ]
        div [ ClassName "row m-2"
              Style [ Height "65%" ] ] [
            (displayCurrentCard model dispatch)
            (sidebar model dispatch)
        ]
        (activeCards model dispatch)
        (settings model dispatch)
    ]

Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run
