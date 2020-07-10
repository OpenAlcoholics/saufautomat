module App

(**
 The famous Increment/Decrement ported from Elm.
 You can find more info about Elmish architecture and samples at https://elmish.github.io/
*)

open Card
open Elmish
open Elmish.React
open Browser
open Fable.Core
open Fable.React
open Fable.React.Props
open Player
open Resources
open System.Text.RegularExpressions
open Thoth.Fetch

// MODEL

type Settings =
    { MinimumSips: int
      MaximumSips: int
      Remote: bool
      Audio: bool
      Language: string }

type RoundInformation =
    { CardsToPlay: int
      InitialPlayerIndex: int }

type Model =
    { Players: Player.Type list
      ActiveCards: (Card.Type * Player.Type option) list
      CurrentCard: Card.Type option
      Cards: Card.Type list
      CurrentPlayer: Player.Type option
      Counter: int
      DisplayPlayerNameDuplicateError: bool
      InitialLoad: bool
      Settings: Settings
      Round: int
      RoundInformation: RoundInformation }

type Msg =
    | InitialLoad
    | ChangeActiveCard
    | ChangeActivePlayer
    | IncrementCounter
    | AddActiveCard of Card.Type * Player.Type option
    | AddCards of Card.RawType list
    | AddPlayer of Player.Type
    | RemovePlayer of Player.Type
    | TogglePlayerActivity of Player.Type
    | DisplayPlayerNameDuplicate
    | HidePlayerNameDuplicate
    | DecrementActiveRoundCards
    | DecrementPlayerRoundCards
    | UseActiveCard of Card.Type * Player.Type
    | SaveSettings
    | ChangeAudioSetting
    | ChangeRemoteSetting
    | Reset
    | AdvanceTurn
    | AdvanceRound
    | ChangeLanguage of string
    | PlayAudio
    | RemoveActiveCard of Card.Type
    | RemoveCardFromSession of Card.Type

let getCards language dispatch =
    promise {
        let url =
            sprintf
                "https://raw.githubusercontent.com/OpenAlcoholics/drinking-game-cards/feature/i18n/minified_%s.json"
                language
        let! res = Fetch.get (url)
        AddCards res |> dispatch
    }

let play id =
    ((Browser.Dom.window.document.getElementById id) :?> Browser.Types.HTMLMediaElement).play()

[<Emit("$0.currentTime = $2")>]
let assignCurrentTime element value = jsNative

let stop id =
    ((Browser.Dom.window.document.getElementById id) :?> Browser.Types.HTMLMediaElement).pause()
    assignCurrentTime ((Browser.Dom.window.document.getElementById id) :?> Browser.Types.HTMLMediaElement) "0.0"

let findCookieValue (name: string): string option =
    let kvArrToPair (kvArr: string []): string * string =
        match kvArr with
        | [| k; v |] -> (k, v)
        | _ -> ("", "")

    let rawCookies: string = Browser.Dom.document.cookie
    rawCookies.Split ';'
    |> Array.map (fun (s: string) -> s.Trim().Split '=' |> kvArrToPair)
    |> Map.ofArray
    |> Map.tryFind name

type HtmlAttr =
    | [<CompiledName("aria-valuenow")>] AriaValueNow of string
    | [<CompiledName("aria-valuemin")>] AriaValueMin of string
    | [<CompiledName("aria-valuemax")>] AriaValueMax of string
    | [<CompiledName("data-toggle")>] DataToggle of string
    | [<CompiledName("data-target")>] DataTarget of string
    | [<CompiledName("data-dismiss")>] DataDismiss of string
    | [<CompiledName("type")>] InputType of string
    | [<CompiledName("for")>] For of string
    interface IHTMLProp

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
            MaximumSips = (unwrapOr (findCookieValue "maximum-sips") "10") |> int
            Remote = (unwrapOr (findCookieValue "remote") "true") = "true"
            Audio = (unwrapOr (findCookieValue "audio") "true") = "true"
            Language =
                (unwrapOr (findCookieValue "language") (Seq.head ((unwrapOr navigator.language "en-US").Split '-'))) }
      Round = 0
      RoundInformation =
          { CardsToPlay = 0
            InitialPlayerIndex = -1 } }, Cmd.Empty

let unwrapMapOrDefault (opt: 'b option) (m: 'b -> 't) (def: 't) =
    if opt.IsSome then m opt.Value else def

let unwrapOr (opt: 'b option) (def: 'b): 'b =
    match opt with
    | Some value -> value
    | None -> def

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
                let player = playerList.Item((index + 1) % playerList.Length)
                match player.Active with
                | false -> findNextActivePlayer (List.filter ((<>) player) playerList) model
                | true -> Some player
            | None -> Some model.Players.Head
        | None -> Some model.Players.Head

// UPDATE

let int_replacement_regex = Regex("{int(:\s*[a-z]+)?}")

let filterCardsForTurn cards model =
    let distinctCount = (Card.getDistinctCount cards)

    let cards =
        List.filter (fun card ->
            card.Count > 0 && ((model.Settings.Remote && card.Remote) || (not model.Settings.Remote))
            && if model.Players.Length = 0 then
                (not card.Personal) && card.Rounds = 0
               else
                   true && if distinctCount > 1 // TODO: this should be checked after everything else
                           then card.Id <> (unwrapMapOrDefault model.CurrentCard (fun c -> c.Id) -1)
                           else true) cards

    List.filter (fun card ->
        if card.Unique && not card.Personal
        then (List.filter (fun (activeCard, _) -> card.Unique && (activeCard = card)) model.ActiveCards).Length = 0
        else true) cards

let getNextCard cards model =
    let cards = filterCardsForTurn cards model
    if cards.Length = 0 then
        None
    else
        let card = cards.Item(System.Random().Next() % cards.Length)

        let min = model.Settings.MinimumSips
        let max = model.Settings.MaximumSips

        let text = card.Text.Replace("{int: i}", "{int}").Replace("{int: j}", "{int}").Replace("{int: k}", "{int}")

        let replacement_text =
            (Seq.map (fun (w: string) ->
                // This is temporary until the parser and variable replacement is implemented
                let m = int_replacement_regex.Match w
                match m.Success with
                | true -> (sprintf "%d" ((System.Random().Next()) % (max - min + 1) + min))
                | false -> w) (text.Split ' '))
            |> String.concat " "
        Some { card with Text = replacement_text }

let explodeCards cards =
    (List.map (fun card -> ([ card ] |> Seq.collect (fun c -> List.replicate c.Count { c with Count = 1 }))) cards)
    |> Seq.reduce Seq.append
    |> List.ofSeq

let roundHasEnded model =
    model.RoundInformation.CardsToPlay <= 0 && model.Players.Length > 0

let getPlayerByIndex index (players: Player.Type list): Player.Type option =
    try
        Some(players.Item index)
    with _ -> None

let allowedLanguages = [ "de"; "en" ]

let update (msg: Msg) (model: Model) =
    match msg with
    | InitialLoad ->
        { model with InitialLoad = false },
        Cmd.ofSub (fun dispatch -> getCards model.Settings.Language dispatch |> Promise.start)
    | AdvanceTurn ->
        model,
        Cmd.ofSub (fun dispatch ->
            do dispatch PlayAudio
               dispatch IncrementCounter
               dispatch ChangeActivePlayer
               dispatch ChangeActiveCard
               dispatch DecrementPlayerRoundCards
               if roundHasEnded model then dispatch AdvanceRound)
    | PlayAudio ->
        if model.Settings.Audio then
            let audioId =
                (if roundHasEnded model then "nextround-audio" else "nextcard-audio")

            stop "nextcard-audio"
            stop "nextround-audio"
            play audioId

        model, Cmd.Empty
    | ChangeActiveCard ->
        let playerCards = Player.filterActiveCards (model.CurrentPlayer.Value) (model.ActiveCards)

        let cards =
            if model.CurrentPlayer.IsSome
            then List.filter (fun card -> not (List.exists (fun c -> c = card) playerCards)) model.Cards
            else model.Cards

        let card = getNextCard cards model

        let model =
            { model with
                  CurrentCard = card
                  Cards = Card.decreaseCount card model.Cards
                  RoundInformation =
                      { model.RoundInformation with CardsToPlay = max (model.RoundInformation.CardsToPlay - 1) 0 } }

        model,
        (if card.IsSome then
            Cmd.ofSub (fun dispatch ->
                (if (card.IsSome && card.Value.Rounds <> 0) then
                    AddActiveCard
                        (card.Value,
                         (if card.Value.Personal then model.CurrentPlayer else None))
                    |> dispatch))
         else
             Cmd.Empty)
    | ChangeActivePlayer ->
        let nextPlayer =
            findNextActivePlayer
                ((List.filter (fun p -> p.Active || (Player.compareOption (model.CurrentPlayer) (Some p)))
                      model.Players)) model

        (if model.Counter <> 0 then
            { model with
                  CurrentPlayer = nextPlayer
                  Players =
                      List.map (fun player ->
                          if (Player.compareOption (Some player) nextPlayer)
                          then { player with CardsPlayed = player.CardsPlayed + 1 }
                          else player) model.Players }
         else
             model), Cmd.Empty
    | IncrementCounter ->
        { model with Counter = model.Counter + 1 }, Cmd.Empty
    | AddCards cards ->
        { model with Cards = explodeCards (List.map Card.Into cards) }, Cmd.Empty
    | AddPlayer player ->
        { model with
              Players = model.Players @ [ player ]
              RoundInformation = { model.RoundInformation with CardsToPlay = model.RoundInformation.CardsToPlay + 1 } },
        match model.CurrentPlayer with
        | Some _ -> Cmd.Empty
        | None -> Cmd.ofSub (fun dispatch -> dispatch ChangeActivePlayer)
    | RemovePlayer player ->
        let players = (List.filter (fun p -> p <> player) model.Players)
        let activeCards =
            List.filter (fun (_, p) -> not (Player.compareOption p (Some player))) model.ActiveCards

        { model with
              Players = players
              ActiveCards = activeCards
              RoundInformation = { model.RoundInformation with CardsToPlay = model.RoundInformation.CardsToPlay - 1 } },
        (if (Player.isCurrent player model.CurrentPlayer)
         then Cmd.ofSub (fun dispatch -> dispatch AdvanceTurn)
         else Cmd.Empty)
    | TogglePlayerActivity player ->

        { model with
              Players =
                  (List.map (fun p ->
                      if p = player then { p with Active = not p.Active } else p) model.Players) },
        (if (Player.isCurrent player model.CurrentPlayer)
         then Cmd.ofSub (fun dispatch -> dispatch AdvanceTurn)
         else Cmd.Empty)
    | DisplayPlayerNameDuplicate -> { model with DisplayPlayerNameDuplicateError = true }, Cmd.Empty
    | HidePlayerNameDuplicate -> { model with DisplayPlayerNameDuplicateError = false }, Cmd.Empty
    | AddActiveCard (card, player) -> { model with ActiveCards = (card, player) :: model.ActiveCards }, Cmd.Empty
    | DecrementActiveRoundCards ->
        { model with
              ActiveCards =
                  (List.filter (fun (card, _) -> card.Rounds <> 0)
                       (List.map (fun (card, player: Player.Type option) ->
                           { card with
                                 Rounds =
                                     if card.Rounds > 0 && card.Uses = 0 && player.IsNone
                                     then card.Rounds - 1
                                     else card.Rounds }, player) model.ActiveCards)) }, Cmd.Empty
    | DecrementPlayerRoundCards ->
        { model with
              ActiveCards =
                  List.filter (fun (c, _) -> (c.Rounds <> 0 || c.Uses > 0))
                      (List.map (fun (c, p) ->
                          (if Player.compareOption model.CurrentPlayer p
                           then { c with Rounds = c.Rounds - 1 }
                           else c), p) model.ActiveCards) }, Cmd.Empty
    | UseActiveCard (card, player) ->
        { model with
              ActiveCards =
                  List.filter (fun (c, p) ->
                      (c.Rounds <> 0 || c.Uses > 0)
                      && not (card.Uses = 1 && card = c && (Player.compareOption (Some player) p)))
                      (List.map (fun (c, p) ->
                          (if card = c && (Player.compareOption (Some player) p)
                           then { c with Uses = c.Uses - 1 }
                           else c), p) model.ActiveCards) },
        Cmd.ofSub (fun dispatch -> AddActiveCard({ card with Uses = 0 }, (Some player)) |> dispatch)
    | ChangeRemoteSetting ->
        let remote =
            ((Browser.Dom.window.document.getElementById "remote") :?> Browser.Types.HTMLInputElement).``checked``

        { model with Settings = { model.Settings with Remote = remote } }, Cmd.Empty
    | ChangeAudioSetting ->
        let audio =
            ((Browser.Dom.window.document.getElementById "audio") :?> Browser.Types.HTMLInputElement).``checked``

        { model with Settings = { model.Settings with Audio = audio } }, Cmd.Empty
    | SaveSettings ->
        let min =
            (match ((Browser.Dom.window.document.getElementById "minimum-sips") :?> Browser.Types.HTMLInputElement).value with
             | "" -> model.Settings.MinimumSips
             | value -> value |> int)

        let max =
            (match ((Browser.Dom.window.document.getElementById "maximum-sips") :?> Browser.Types.HTMLInputElement).value with
             | "" -> model.Settings.MaximumSips
             | value -> value |> int)

        let language =
            (match ((Browser.Dom.window.document.getElementById "language") :?> Browser.Types.HTMLInputElement).value with
             | "" -> model.Settings.Language
             | value -> value)

        let language =
            if not (List.exists ((=) language) allowedLanguages)
            then model.Settings.Language
            else language

        { model with
              Settings =
                  { model.Settings with
                        MinimumSips = min
                        MaximumSips = max } },
        (if language <> model.Settings.Language
         then Cmd.ofSub (fun dispatch -> ChangeLanguage language |> dispatch)
         else Cmd.Empty)
    | Reset -> init ()
    | AdvanceRound ->
        (if model.Players.Length > 0 then
            { model with
                  Round = model.Round + 1
                  RoundInformation =
                      { CardsToPlay = (Player.getActive model.Players).Length - 1
                        InitialPlayerIndex = (unwrapOr (Player.getIndex model.CurrentPlayer model.Players) 0) - 1 } }
         else
             model), Cmd.ofSub (fun dispatch -> dispatch DecrementActiveRoundCards)
    | RemoveActiveCard card ->
        { model with ActiveCards = List.filter (fun (c, _) -> card <> c) model.ActiveCards }, Cmd.Empty
    | RemoveCardFromSession card ->
        { model with Cards = List.filter (fun c -> card <> c) model.Cards },
        Cmd.ofSub (fun dispatch -> dispatch AdvanceTurn)
    | ChangeLanguage language ->
        { model with
              Settings = { model.Settings with Language = language }
              ActiveCards = [] }, Cmd.ofSub (fun dispatch -> getCards language dispatch |> Promise.start)

// VIEW (rendered with React)

let settings model dispatch =
    div
        [ ClassName "modal fade"
          Id "settings"
          TabIndex -1
          Role "dialog" ]
        [ div
            [ ClassName "modal-dialog"
              Role "document" ]
              [ div [ ClassName "modal-content" ]
                    [ div [ ClassName "modal-body" ]
                          [ div [ ClassName "form-group container" ]
                                [ div [ ClassName "row" ]
                                      [ label
                                          [ For "minimum-sips"
                                            ClassName "col align-self-center" ]
                                            [ str (getKey (model.Settings.Language) "SETTINGS_MINIMUM_SIPS") ]
                                        input
                                            [ Name "minimum-sips"
                                              ClassName "m-1 w-100 col"
                                              Id "minimum-sips"
                                              Placeholder(sprintf "%d" (model.Settings.MinimumSips))
                                              MaxLength 2.
                                              InputType "text"
                                              Pattern "\d{1,2}" ] ]
                                  div [ ClassName "row" ]
                                      [ label
                                          [ For "maximum-sips"
                                            ClassName "col align-self-center" ]
                                            [ str (getKey (model.Settings.Language) "SETTINGS_MAXIMUM_SIPS") ]
                                        input
                                            [ Name "maximum-sips"
                                              ClassName "m-1 w-100 col"
                                              Id "maximum-sips"
                                              Placeholder(sprintf "%d" (model.Settings.MaximumSips))
                                              MaxLength 2.
                                              InputType "text"
                                              Pattern "\d{1,2}" ] ]
                                  div [ ClassName "row" ]
                                      [ label
                                          [ For "remote"
                                            ClassName "col align-self-center" ]
                                            [ str (getKey (model.Settings.Language) "SETTINGS_REMOTE") ]
                                        input
                                            [ Name "remote"
                                              OnClick(fun _ -> dispatch ChangeRemoteSetting)
                                              InputType "checkbox"
                                              ClassName "m-1 w-100 col"
                                              Id "remote"
                                              Checked(model.Settings.Remote) ] ]
                                  div [ ClassName "row" ]
                                      [ label
                                          [ For "audio"
                                            ClassName "col align-self-center" ]
                                            [ str (getKey (model.Settings.Language) "SETTINGS_AUDIO") ]
                                        input
                                            [ Name "audio"
                                              OnClick(fun _ -> dispatch ChangeAudioSetting)
                                              InputType "checkbox"
                                              ClassName "m-1 w-100 col"
                                              Id "audio"
                                              Checked(model.Settings.Audio) ] ]
                                  div [ ClassName "row" ]
                                      [ label
                                          [ For "language"
                                            ClassName "col align-self-center" ]
                                            [ str (getKey (model.Settings.Language) "SETTINGS_LANGUAGE") ]
                                        select
                                            [ Name "language"
                                              ClassName "m-1 w-100 col"
                                              Id "language" ]
                                            (List.map
                                                (fun language ->
                                                    option [ Selected(language = model.Settings.Language) ]
                                                        [ str language ]) allowedLanguages) ] ] ]
                      div [ ClassName "modal-footer" ]
                          [ span [ ClassName "text-secondary" ] [ str "{{TAG}}" ]
                            button
                                [ ClassName "btn btn-primary"
                                  DataDismiss "modal"
                                  OnClick(fun _ -> dispatch SaveSettings) ]
                                [ str (getKey (model.Settings.Language) "SETTINGS_SAVE") ] ] ] ] ]

let joinHtmlElements (sep: ReactElement) (l: ReactElement list) =
    Seq.ofList l
    |> Seq.fold (fun acc y ->
        if Seq.isEmpty acc
        then seq { y }
        else Seq.append acc (Seq.ofList [ sep; y ])) Seq.empty
    |> List.ofSeq

let addPlayer name model dispatch =
    match List.tryFind ((=) (Player.create name)) model.Players with
    | Some _ -> false
    | None ->
        dispatch (AddPlayer(Player.create name))
        HidePlayerNameDuplicate |> ignore
        true

let addPlayerFunction model dispatch =
    match ((Browser.Dom.window.document.getElementById "add-player-field") :?> Browser.Types.HTMLInputElement).value with
    | "" -> ()
    | value ->
        (match (addPlayer value model dispatch) with
         | true ->
             ((Browser.Dom.window.document.getElementById "add-player-field") :?> Browser.Types.HTMLInputElement).value <- ""
         | false ->
             DisplayPlayerNameDuplicate |> dispatch)

let displayPlayer player model dispatch =
    div
        [ ClassName
            ("flex-column mr-2 card " + (if model.CurrentPlayer.IsSome && model.CurrentPlayer.Value.Name = player.Name
                                         then "bg-primary"
                                         else if not player.Active
                                         then "border-warning"
                                         else "")) ]
        [ div [ ClassName "card-body" ]
              [ h5 [ ClassName "card-title text-center" ] [ str (sprintf "%s (%d)" player.Name player.CardsPlayed) ]
                div [ ClassName "d-flex justify-content-between" ]
                    [ button
                        [ ClassName "card btn btn-secondary toggle-button"
                          OnClick(fun _ -> TogglePlayerActivity player |> dispatch) ]
                          [ str
                              (if player.Active
                               then (getKey (model.Settings.Language) "PLAYER_SUSPEND_ON")
                               else (getKey (model.Settings.Language) "PLAYER_SUSPEND_OFF")) ]
                      button
                          [ ClassName "card btn btn-secondary delete-button"
                            OnClick(fun _ -> RemovePlayer player |> dispatch) ]
                          [ str (getKey (model.Settings.Language) "PLAYER_DELETE") ] ] ] ]

let sidebar (model: Model) dispatch =
    div [ ClassName "col-md-2 sidebar col h-100 d-none d-md-block d-lg-block d-xl-block" ]
        [ div [ ClassName "form-group" ]
              [ input
                  [ Name "add-player-field"
                    ClassName "form-control m-1 w-100"
                    Id "add-player-field"
                    OnKeyDown(fun x ->
                        if x.keyCode = 13. then (addPlayerFunction model dispatch))
                    MaxLength 20. ]
                button
                    [ ClassName "btn btn-primary m-1 w-100"
                      OnClick(fun _ -> addPlayerFunction model dispatch) ]
                    [ str (getKey (model.Settings.Language) "ADD_PLAYER") ] ]
          hr []
          div
              [ ClassName "flex-row mb-4"
                Style
                    [ OverflowY "scroll"
                      Height "85%" ] ] (List.map (fun p -> displayPlayer p model dispatch) model.Players) ]

let displayCurrentCard model dispatch =
    div
        [ ClassName "card d-flex col"
          Id "active-card" ]
        [ div [ ClassName "card-body flex-wrap" ]
              [ button
                  [ OnClick(fun _ -> dispatch AdvanceTurn)
                    ClassName "card-body card-title btn btn-dark w-100"
                    Style [ Height "95%" ]
                    Id "current-card-body"
                    Disabled(model.CurrentCard.IsNone && model.Counter > 0) ]
                    [ span [ ClassName "h3" ]
                          [ str
                              (match model.CurrentCard with
                               | Some (card) -> card.Text
                               | None ->
                                   if model.Counter = 0
                                   then (getKey (model.Settings.Language) "CLICK_TO_START")
                                   else (getKey (model.Settings.Language) "NO_CARDS_LEFT")) ] ]
                button
                    [ ClassName "btn btn-secondary d-none d-md-block d-lg-block d-xl-block"
                      Disabled model.CurrentCard.IsNone
                      OnClick(fun _ ->
                          if model.CurrentCard.IsSome then RemoveCardFromSession model.CurrentCard.Value |> dispatch) ]
                    [ str (getKey (model.Settings.Language) "DELETE_CARD_FROM_SESSION") ]
                (if model.CurrentCard.IsSome && model.CurrentCard.Value.Personal then
                    span
                        [ ClassName "badge badge-secondary ml-2"
                          Style [ FontSize "0.9rem" ] ] [ str "personal" ]
                 else
                     span [] []) ] ]

let progressBarHeader model =
    [ div [ ClassName "progress" ]
          [ div
              [ ClassName "progress-bar"
                Role "progressbar"
                Style([ Width(sprintf "%d%%" ((model.Counter * 100 / model.Cards.Length))) ])
                AriaValueNow(sprintf "%d" model.Counter)
                AriaValueMin "0"
                AriaValueMax(sprintf "%d" model.Cards.Length) ] [] ] ]

let displayInformationHeader model =
    let separator: ReactElement = span [] [ str " | " ]

    let elements =
        [ (span []
               [ str
                   (match model.CurrentPlayer with
                    | Some player -> player.Name
                    | None -> (getKey (model.Settings.Language) "NO_ACTIVE_PLAYER")) ])
          (span [ Title(getKey (model.Settings.Language) "NUMBER_CARDS_PLAYED") ]
               [ str (sprintf "%s %d" (getKey (model.Settings.Language) "CARDS_PLAYED") model.Counter) ])
          (span [] [ str (sprintf "%s %d" (getKey (model.Settings.Language) "ROUND") model.Round) ]) ]

    div
        [ Id "active-player-header"
          ClassName "text-center col text-truncate h3 d-none d-md-block d-lg-block d-xl-block" ] (joinHtmlElements separator elements)

let displayActiveCard (card, player: Player.Type option) model dispatch =
    div
        [ ClassName
            ("card p-2 m-1" + (if Player.compareOption player model.CurrentPlayer
                               then " border-success"
                               else ""))
          Title card.Text ]
        [ h5 [ ClassName "card-title h-100" ]
              [ str
                  ((if player.IsSome then (sprintf "[%s] " player.Value.Name) else "") + card.Text
                   + (if card.Rounds > 0 && card.Uses = 0 then (sprintf " (%d)" card.Rounds) else "")) ]
          (if card.Uses > 0 && player.IsSome then
              button
                  [ ClassName "card btn btn-primary"
                    OnClick(fun _ -> UseActiveCard(card, player.Value) |> dispatch) ]
                  [ str (sprintf "%s (%d)" (getKey (model.Settings.Language) "ACTIVE_CARD_USE") card.Uses) ]
           else
               span [] [])
          button
              [ ClassName "btn btn-primary"
                OnClick(fun _ -> RemoveActiveCard card |> dispatch) ]
              [ str (getKey (model.Settings.Language) "ACTIVE_CARD_DELETE") ] ]

let activeCards (model: Model) dispatch =
    div
        [ ClassName "active-cards row mt-3"
          Style
              [ Height "22%"
                OverflowY "scroll"
                MarginRight "1.67em" ] ]
        [ div [ ClassName "col d-flex flex-wrap" ]
              (List.map (fun card -> displayActiveCard card model dispatch)
                   (List.rev
                       (List.sortBy (fun (_, player) -> Player.compareOption model.CurrentPlayer player)
                            model.ActiveCards))) ]

let view (model: Model) dispatch =
    div
        [ Ref(fun element ->
            if not (isNull element) then
                if model.InitialLoad then dispatch InitialLoad)
          ClassName "container-fluid h-100" ]
        [ div [ ClassName "row m-4" ]
              [ figure []
                    [ audio
                        [ Id "nextcard-audio"
                          Src "/nextcard.mp3" ] [] ]
                figure []
                    [ audio
                        [ Id "nextround-audio"
                          Src "/nextround.mp3" ] [] ]
                div [ ClassName "col-1" ]
                    [ button
                        [ ClassName "btn btn-primary m-1"
                          DataToggle "modal"
                          DataTarget "#settings" ] [ str (getKey (model.Settings.Language) "SETTINGS") ]
                      button
                          [ ClassName "btn btn-primary ml-1"
                            OnClick(fun _ -> dispatch Reset) ] [ str (getKey (model.Settings.Language) "RESET") ] ]
                displayInformationHeader model
                span [ ClassName "text-secondary d-none d-md-block d-lg-block d-xl-block" ]
                    [ str (sprintf "%s: saufautomat@carstens.tech" (getKey (model.Settings.Language) "CONTACT")) ] ]
          div
              [ ClassName "row m-2"
                Style [ Height "65%" ] ]
              [ (displayCurrentCard model dispatch)
                (sidebar model dispatch) ]
          (activeCards model dispatch)
          (settings model dispatch) ]

// App
Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run
