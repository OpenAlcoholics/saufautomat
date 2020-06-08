module App

(**
 The famous Increment/Decrement ported from Elm.
 You can find more info about Elmish architecture and samples at https://elmish.github.io/
*)

open Elmish
open Elmish.React
open Browser
open Fable.Core
open Fable.React
open Fable.React.Props
open Player
open Thoth.Fetch
open Thoth.Json
open System
open System.Text.RegularExpressions

// MODEL

type RawCard =
    { id: int
      text: string
      count: int
      uses: int
      rounds: int
      personal: bool
      remote: bool
      unique: bool }

type Settings =
    { MinimumSips: int
      MaximumSips: int }

type RoundInformation = {
    CardsToPlay: int
}

type Model =
    { Players: Player.Type list
      ActiveCards: RawCard list
      CurrentCard: RawCard option
      Cards: RawCard list
      CurrentPlayer: Player.Type option
      Counter: int
      DisplayPlayerNameDuplicateError: bool
      InitialLoad: bool
      Settings: Settings
      Round: int
      RoundInformation : RoundInformation }

type Msg =
    | InitialLoad
    | ChangeActiveCard
    | ChangeActivePlayer
    | IncrementCounter
    | AddActiveCard of RawCard
    | AddCards of RawCard list
    | AddPlayer of Player.Type
    | RemovePlayer of Player.Type
    | TogglePlayerActivity of Player.Type
    | DisplayPlayerNameDuplicate
    | HidePlayerNameDuplicate
    | DecrementActiveRoundCards
    | DecrementPlayerUseCards
    | SaveSettings
    | Reset
    | IncrementRound

let getCards dispatch =
    promise {
        let url = "https://raw.githubusercontent.com/OpenAlcoholics/drinking-game-cards/develop/tasks.json"
        let! res = Fetch.get (url)
        AddCards res |> dispatch
    }

//[<Emit("(new Audio($0)).play();")>]
//let play (fileName: string) = jsNative
let play () =
    ((Browser.Dom.window.document.getElementById "nextcard-audio") :?> Browser.Types.HTMLMediaElement).play()

[<Emit("$0.currentTime = $2")>]
let assignCurrentTime element value = jsNative

let stop () =
    ((Browser.Dom.window.document.getElementById "nextcard-audio") :?> Browser.Types.HTMLMediaElement).pause()
    assignCurrentTime
        ((Browser.Dom.window.document.getElementById "nextcard-audio") :?> Browser.Types.HTMLMediaElement) "0.0"


type HtmlAttr =
    | [<CompiledName("aria-valuenow")>] AriaValueNow of string
    | [<CompiledName("aria-valuemin")>] AriaValueMin of string
    | [<CompiledName("aria-valuemax")>] AriaValueMax of string
    | [<CompiledName("data-toggle")>] DataToggle of string
    | [<CompiledName("data-target")>] DataTarget of string
    | [<CompiledName("data-dismiss")>] DataDismiss of string
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
          { MinimumSips = 2
            MaximumSips = 10 }
      Round = 0
      RoundInformation = {
          CardsToPlay = 0
      }}, Cmd.Empty

let getDistinctCardCount cards =
    (List.map (fun c -> c.id) cards |> List.distinct).Length

let unwrapOrMap (opt: 'b option) (m: 'b -> 't) (def: 't) =
    if opt.IsSome then m opt.Value else def

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

let int_replacement_regex = new Regex("{int(:?:(\d+)-(\d+))?}")

let filterCardsForTurn model =
    let distinctCount = (getDistinctCardCount model.Cards)

    let cards =
        List.filter (fun card ->
            card.count > 0 && if model.Players.Length = 0 then
                                  not card.personal
                              else
                                  true && if distinctCount > 1 // TODO: this should be checked after everything else
                                          then card.id <> (unwrapOrMap model.CurrentCard (fun c -> c.id) -1)
                                          else true) model.Cards

//    List.filter
//        (fun card ->
//            (List.filter (fun activeCard -> if card.unique then not (activeCard.id = card.id) else true) model.ActiveCards).Length = 0)
//        cards
    cards

let getNextCard model =
    let cards = filterCardsForTurn model
    if cards.Length = 0 then
        None
    else
        let card = cards.Item(System.Random().Next() % cards.Length)

        let min = model.Settings.MinimumSips
        let max = model.Settings.MaximumSips

        let replacement_text =
            (Seq.map (fun w ->
                let m = int_replacement_regex.Match w
                match m.Success with
                | true -> (sprintf "%d" ((System.Random().Next()) % (max - min + 1) + min))
                | false -> w) (card.text.Split ' '))
            |> String.concat " "
        Some { card with text = replacement_text }

let decreaseCardCount card cards =
    match card with
    | Some card ->
        List.map (fun c ->
            if c.id = card.id then { c with count = c.count - 1 } else c) cards
    | None -> cards

let explodeCards cards =
    (List.map (fun card -> ([ card ] |> Seq.collect (fun c -> List.replicate c.count { c with count = 1 }))) cards)
    |> Seq.reduce Seq.append
    |> List.ofSeq

let roundHasEnded model =
    model.RoundInformation.CardsToPlay = 0

let getPlayerIndex player players =
    match player with
    | Some p -> List.tryFindIndex p players
    | None -> None

let getActivePlayers model =
    List.filter (fun player -> player.Active) model.Players

let update (msg: Msg) (model: Model) =
    match msg with
    | InitialLoad ->
        { model with InitialLoad = false }, Cmd.ofSub (fun dispatch -> getCards dispatch |> Promise.start)
    | ChangeActiveCard ->
        let card = getNextCard model
        stop ()
        play ()
        { model with
              CurrentCard = card
              Cards = decreaseCardCount card model.Cards
              RoundInformation = { CardsToPlay = model.RoundInformation.CardsToPlay - 1 } },
        (if card.IsSome then
            Cmd.ofSub (fun dispatch ->
                do dispatch IncrementCounter
                   // dispatch DecrementPlayerUseCards
                   (if (card.IsSome && card.Value.rounds <> 0) then AddActiveCard card.Value |> dispatch))
         else
             Cmd.Empty)
    | ChangeActivePlayer ->
        let nextPlayer =
            findNextActivePlayer
                (List.filter (fun p -> p.Active || (Player.compareOption (model.CurrentPlayer) (Some p))) model.Players)
                model

        { model with
              CurrentPlayer = nextPlayer
              Players =
                  List.map (fun player ->
                      if (Player.compareOption (Some player) nextPlayer)
                      then { player with CardsPlayed = player.CardsPlayed + 1 }
                      else player) model.Players }, if (roundHasEnded model) then Cmd.ofSub(fun dispatch -> dispatch IncrementRound) else Cmd.Empty
    | IncrementCounter ->
        { model with Counter = model.Counter + 1 }, Cmd.Empty
    | AddCards cards ->
        { model with Cards = (explodeCards cards) }, Cmd.Empty
    | AddPlayer player ->
        { model with Players = model.Players @ [ player ]
                     RoundInformation = { CardsToPlay = model.RoundInformation.CardsToPlay + 1 (*TODO: should this only be executed when the added player has a turn in this round?*) } },
        match model.CurrentPlayer with
        | Some _ -> Cmd.Empty
        | None -> Cmd.ofSub (fun dispatch -> dispatch ChangeActivePlayer)
    | RemovePlayer player ->
        { model with
              Players = (List.filter (fun p -> p <> player) model.Players)
              RoundInformation = { CardsToPlay = model.RoundInformation.CardsToPlay - 1 }
              CurrentPlayer =
                  match model.CurrentPlayer with
                  | Some currentPlayer ->
                      if player = currentPlayer then None else model.CurrentPlayer
                  | None -> None }, Cmd.Empty
    | TogglePlayerActivity player ->
        { model with
              Players =
                  (List.map (fun p ->
                      if p = player then { p with Active = not p.Active } else p) model.Players) }, Cmd.Empty
    | DisplayPlayerNameDuplicate -> { model with DisplayPlayerNameDuplicateError = true }, Cmd.Empty
    | HidePlayerNameDuplicate -> { model with DisplayPlayerNameDuplicateError = false }, Cmd.Empty
    | AddActiveCard card -> { model with ActiveCards = card :: model.ActiveCards }, Cmd.Empty
    | DecrementActiveRoundCards ->
        { model with
              ActiveCards =
                  (List.filter (fun card -> card.rounds <> 0)
                       (List.map (fun card -> { card with rounds = card.rounds - 1 }) model.ActiveCards)) }, Cmd.Empty
    | DecrementPlayerUseCards -> model, Cmd.Empty // TODO
    | SaveSettings ->
        let min =
            (match ((Browser.Dom.window.document.getElementById "minimum-sips") :?> Browser.Types.HTMLInputElement).value with
             | "" -> model.Settings.MinimumSips
             | value -> value |> int)

        let max =
            (match ((Browser.Dom.window.document.getElementById "maximum-sips") :?> Browser.Types.HTMLInputElement).value with
             | "" -> model.Settings.MaximumSips
             | value -> value |> int)

        { model with
              Settings =
                  { model.Settings with
                        MinimumSips = min
                        MaximumSips = max } }, Cmd.Empty
    | Reset -> init ()
    | IncrementRound -> { model with Round = model.Round + 1
                                     RoundInformation = { CardsToPlay = (getActivePlayers model).Length } }, Cmd.Empty

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
                                            ClassName "col" ] [ str "Minimum sips" ]
                                        input
                                            [ Name "minimum-sips"
                                              ClassName "m-1 w-100 col"
                                              Id "minimum-sips"
                                              Placeholder(sprintf "%d" (model.Settings.MinimumSips))
                                              MaxLength 2. ] ]
                                  div [ ClassName "row" ]
                                      [ label
                                          [ For "maximum-sips"
                                            ClassName "col" ] [ str "Maximum sips" ]
                                        input
                                            [ Name "maximum-sips"
                                              ClassName "m-1 w-100 col"
                                              Id "maximum-sips"
                                              Placeholder(sprintf "%d" (model.Settings.MaximumSips))
                                              MaxLength 2. ] ] ] ]
                      div [ ClassName "modal-footer" ]
                          [ span [ ClassName "text-secondary" ] [ str "{{TAG}}" ]
                            button
                              [ ClassName "btn btn-primary"
                                DataDismiss "modal"
                                OnClick(fun _ -> dispatch SaveSettings) ] [ str "Save" ] ] ] ] ]

let addPlayer name model dispatch =
    dispatch (AddPlayer(Player.create name))
    HidePlayerNameDuplicate |> ignore
    true

let tryAddPlayer name model dispatch =
    match List.tryFind ((=) (Player.create name)) model.Players with
    | Some _ -> false
    | None -> addPlayer name model dispatch

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
                        [ ClassName "card-text btn btn-secondary toggle-button"
                          OnClick(fun _ -> TogglePlayerActivity player |> dispatch) ] [ str "Toggle" ]
                      button
                          [ ClassName "card-text btn btn-secondary delete-button"
                            OnClick(fun _ ->
                                do RemovePlayer player |> dispatch
                                   ChangeActivePlayer |> dispatch) ] [ str "Delete" ] ] ] ]

let addPlayerFunction model dispatch =
    match ((Browser.Dom.window.document.getElementById "add-player-field") :?> Browser.Types.HTMLInputElement).value with
    | "" -> ()
    | value ->
        (match (tryAddPlayer value model dispatch) with
         | true ->
             (do (((Browser.Dom.window.document.getElementById "add-player-field") :?> Browser.Types.HTMLInputElement).value <- "")
                 HidePlayerNameDuplicate |> dispatch)
             |> ignore
         | false ->
             DisplayPlayerNameDuplicate
             |> dispatch
             |> ignore)
        |> ignore

let sidebar (model: Model) dispatch =
    div [ ClassName "col-md-2 sidebar col h-100" ]
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
                      OnClick(fun _ -> addPlayerFunction model dispatch) ] [ str "Add player" ] ]
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
                  [ OnClick(fun _ ->
                      ChangeActiveCard |> dispatch
                      ChangeActivePlayer |> dispatch)
                    ClassName "card-body card-title btn btn-dark w-100 h-100"
                    Id "current-card-body"
                    Disabled(model.CurrentCard.IsNone && model.Counter > 0) ]
                    [ span [ ClassName "h3" ]
                          [ str
                              (match model.CurrentCard with
                               | Some (card) -> card.text
                               | None ->
                                   if model.Counter = 0 then "Click to start" else "No cards left") ] ] ] ]

let displayInformationHeader model dispatch =
    div
        [ Id "active-player-header"
          ClassName "text-center col text-truncate h3" ]
        [ span []
              [ str
                  (match model.CurrentPlayer with
                   | Some player -> player.Name
                   | None -> "No active player") ]
          span [] [ str " | " ]
          span [ Title "Number of cards played so far" ] [ str (sprintf "%d" model.Counter) ]
          span [] [ str " / " ]
          span [ Title "Total number of cards" ] [ str (sprintf "%d " model.Cards.Length) ]
          span [] [ str " " ]
          span [ Title "Distinct number of cards" ] [ str (sprintf "(%d)" (getDistinctCardCount model.Cards)) ]
          span [ ] [ str (sprintf " | Runde %d" model.Round) ]
          div [ ClassName "progress" ]
              [ div
                  [ ClassName "progress-bar"
                    Role "progressbar"
                    Style([ Width(sprintf "%d%%" ((model.Counter * 100 / model.Cards.Length))) ])
                    AriaValueNow(sprintf "%d" model.Counter)
                    AriaValueMin "0"
                    AriaValueMax(sprintf "%d" model.Cards.Length) ] [] ] ]

let displayActiveCard (card: RawCard) model dispatch =
    div
        [ ClassName "card p-2 m-1"
          Title card.text ]
        [ h5 [ ClassName "card-title h-100" ] [ str (sprintf "%s (%d)" card.text card.rounds) ] ]

let activeCards (model: Model) dispatch =
    div
        [ ClassName "active-cards row mr-4 mt-3"
          Style
              [ Height "22%"
                OverflowY "scroll" ] ]
        [ div [ ClassName "col d-flex flex-wrap" ]
              (List.map (fun card -> displayActiveCard card model dispatch) model.ActiveCards) ]

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
                div [ ClassName "col-1" ]
                    [ button
                        [ ClassName "btn btn-primary m-1"
                          DataToggle "modal"
                          DataTarget "#settings" ] [ str "Settings" ]
                      button
                          [ ClassName "btn btn-primary ml-1"
                            OnClick(fun _ -> dispatch Reset) ] [ str "Reset" ] ]
                (displayInformationHeader model dispatch)
                span [ ClassName "text-secondary" ] [ str "Contact: saufautomat@carstens.tech" ] ]
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
