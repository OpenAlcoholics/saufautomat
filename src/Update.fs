module Update

open Browser
open Card
open Elmish
open Fable.Import
open Fable.SimpleHttp
open Helper
open Model
open Player
open Thoth.Json

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
                do (if (isActiveCard card) then
                      AddActiveCard(card.Value, (if card.Value.Personal then model.CurrentPlayer else None))
                      |> dispatch)
                   dispatch ResetReview)
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
                       (List.map (fun (card, player: Type option) ->
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
        ((Browser.Dom.window.document.getElementsByTagName "html").Item 0)
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
            getValueFromHtmlInput "card-review-id" "" |> int

        let text =
            getValueFromHtmlInput "card-review-text" ""

        let count =
            getValueFromHtmlInput "card-review-count" ""
            |> int

        let uses =
            getValueFromHtmlInput "card-review-uses" "" |> int

        let rounds =
            getValueFromHtmlInput "card-review-rounds" ""
            |> int

        let personal =
            (getValueFromHtmlInput "card-review-personal" "") = "true"

        let remote =
            (getValueFromHtmlInput "card-review-remote" "") = "true"

        let unique =
            (getValueFromHtmlInput "card-review-unique" "") = "true"

        let note =
            getValueFromHtmlInput "card-review-note" ""

        let review =
            Encode.object [ "id", Encode.int id
                            "text", Encode.string text
                            "count", Encode.int count
                            "uses", Encode.int uses
                            "rounds", Encode.int rounds
                            "personal", Encode.bool personal
                            "remote", Encode.bool remote
                            "unique", Encode.bool unique
                            "note", Encode.string note
                            "branch", Encode.string "feature/i18n" ]

        model, Cmd.ofSub (fun dispatch -> sendReview review dispatch |> Promise.start)
    | FinishReview result ->
        match result with
        | Ok _ -> ()
        | Error e -> window.alert (sprintf "Couldn't process review: %s" (e.ToString()))

        model, Cmd.Empty
    | ResetReview ->
        if model.CurrentCard.IsSome && ((document.getElementById "card-review-id") <> null) then
            assignValueToHtmlInput "card-review-id" (sprintf "%d" model.CurrentCard.Value.Id)
            assignValueToHtmlInput "card-review-text" model.CurrentCard.Value.Text
            assignValueToHtmlInput "card-review-count" (sprintf "%d" model.CurrentCard.Value.Count)
            assignValueToHtmlInput "card-review-uses" (sprintf "%d" model.CurrentCard.Value.Uses)
            assignValueToHtmlInput "card-review-rounds" (sprintf "%d" model.CurrentCard.Value.Rounds)
            assignValueToHtmlInput "card-review-personal" (sprintf "%b" model.CurrentCard.Value.Personal)
            assignValueToHtmlInput "card-review-remote" (sprintf "%b" model.CurrentCard.Value.Remote)
            assignValueToHtmlInput "card-review-unique" (sprintf "%b" model.CurrentCard.Value.Unique)
            assignValueToHtmlInput "card-review-note" (unwrapOr model.CurrentCard.Value.Note "")

        model, Cmd.Empty
