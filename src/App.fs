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
open Thoth.Fetch
open Thoth.Json
open System

// MODEL

type Player =
    { Name: string
      Active: bool }

type RawCard =
    { text: string
      count: int
      uses: int
      rounds: int
      personal: bool
      remote: bool
      unique: bool }

type Model =
    { Players: Player list
      ActiveCards: RawCard list
      CurrentCard: RawCard option
      Cards: RawCard list
      CurrentPlayer: Player option
      Counter: int
      DisplayPlayerNameDuplicateError: bool
      InitialLoad: bool }

type Msg =
    | InitialLoad
    | ChangeActiveCard
    | ChangeActivePlayer
    | IncrementCounter
    | UpdateActiveCard of RawCard
    | AddCards of RawCard list
    | AddPlayer of Player
    | RemovePlayer of Player
    | TogglePlayerActivity of Player
    | DisplayPlayerNameDuplicate
    | HidePlayerNameDuplicate

let getCards dispatch =
    promise {
        let url = "https://raw.githubusercontent.com/OpenAlcoholics/drinking-game-cards/develop/tasks_is.json"
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
      InitialLoad = true }, Cmd.Empty

let playerComp p1 p2 =
    p1.Name = p2.Name


let getDistinctCardCount cards =
    (List.map (fun c -> c.text) cards |> List.distinct).Length


let rec findNextActivePlayer (playerList: Player list) model =
    if playerList.Length = 0 then
        None
    else if playerList.Length = 1 then
        Some playerList.Head
    else
        match model.CurrentPlayer with
        | Some player ->
            match List.tryFindIndex (fun p -> playerComp p player) playerList with
            | Some index ->
                let player = playerList.Item((index + 1) % playerList.Length)
                match player.Active with
                | false -> findNextActivePlayer (List.filter (fun p -> player.Name <> p.Name) playerList) model
                | true -> Some player
            | None -> Some model.Players.Head
        | None -> Some model.Players.Head

let unwrapOr (opt: 'b option) (m: 'b -> 't) (def: 't) =
    if opt.IsSome then m opt.Value else def

// UPDATE

let getNextCard model =
    let distinctCount = (getDistinctCardCount model.Cards)

    let cards =
        List.filter (fun card ->
            card.count > 0 && if model.Players.Length = 0 then
                                  not card.personal
                              else
                                  true && if distinctCount > 1
                                          then card.text <> (unwrapOr model.CurrentCard (fun c -> c.text) "")
                                          else true) model.Cards

    if cards.Length = 0 then
        None
    else
        let card = cards.Item(System.Random().Next() % cards.Length)

        let replacement_text =
            (Seq.map (fun w ->
                if w = "{int}"
                then (sprintf "%d" ((System.Random().Next()) % 9 + 2))
                else w) (card.text.Split ' '))
            |> String.concat " "
        Some { card with text = replacement_text }

let decreaseCardCount card cards =
    match card with
    | Some card ->
        List.map (fun c ->
            if c.text = card.text then { c with count = c.count - 1 } else c) cards
    | None -> cards

let explodeCards cards =
    (List.map (fun card -> ([ card ] |> Seq.collect (fun c -> List.replicate c.count { c with count = 1 }))) cards)
    |> Seq.reduce Seq.append
    |> List.ofSeq

let update (msg: Msg) (model: Model) =
    match msg with
    | InitialLoad ->
        { model with InitialLoad = false }, Cmd.ofSub (fun dispatch -> getCards dispatch |> Promise.start)
    | ChangeActiveCard ->
        let card = getNextCard model
        { model with
              CurrentCard = card
              Cards = decreaseCardCount card model.Cards },
        (if card.IsSome
         then Cmd.ofSub (fun dispatch -> dispatch IncrementCounter)
         else Cmd.Empty)
    | ChangeActivePlayer ->
        { model with
              CurrentPlayer =
                  findNextActivePlayer
                      (List.filter (fun p ->
                          p.Active || (match model.CurrentPlayer with
                                       | Some cp -> cp.Name = p.Name
                                       | None -> false)) model.Players) model }, Cmd.Empty
    | IncrementCounter ->
        { model with Counter = model.Counter + 1 }, Cmd.Empty
    | AddCards cards ->
        { model with Cards = (explodeCards cards) }, Cmd.Empty
    | AddPlayer player ->
        { model with Players = player :: model.Players },
        match model.CurrentPlayer with
        | Some _ -> Cmd.Empty
        | None -> Cmd.ofSub (fun dispatch -> dispatch ChangeActivePlayer)
    | RemovePlayer player ->
        { model with
              Players = (List.filter (fun p -> p.Name <> player.Name) model.Players)
              CurrentPlayer =
                  match model.CurrentPlayer with
                  | Some cplayer ->
                      if player.Name = cplayer.Name then None else model.CurrentPlayer
                  | None -> None }, Cmd.Empty
    | TogglePlayerActivity player ->
        { model with
              Players =
                  (List.map (fun p ->
                      if p.Name = player.Name then { p with Active = not p.Active } else p) model.Players) }, Cmd.Empty
    | DisplayPlayerNameDuplicate -> { model with DisplayPlayerNameDuplicateError = true }, Cmd.Empty
    | HidePlayerNameDuplicate -> { model with DisplayPlayerNameDuplicateError = false }, Cmd.Empty
    | UpdateActiveCard card -> model, Cmd.Empty // TODO

// VIEW (rendered with React)

let addPlayer name model dispatch =
    dispatch
        (AddPlayer
            { Name = name
              Active = true })
    HidePlayerNameDuplicate |> ignore
    true

let tryAddPlayer name model dispatch =
    match List.tryFind (fun p -> p.Name = name) model.Players with
    | Some _ -> false
    | None -> addPlayer name model dispatch

let inline (|?) (a: 'a option) b =
    if a.IsSome then a.Value else b

let displayPlayer player model dispatch =
    div
        [ ClassName
            ("flex-column mr-2 card " + (if model.CurrentPlayer.IsSome && model.CurrentPlayer.Value.Name = player.Name
                                    then "bg-primary"
                                    else if not player.Active
                                    then "border-warning"
                                    else "")) ]
        [ div [ ClassName "card-body" ]
              [ h5 [ ClassName "card-title text-center" ] [ str player.Name ]
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
    div
        [ ClassName "col-md-2 sidebar col h-100" ]
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
          div [ ClassName "flex-row mb-4"
                Style [ OverflowY "scroll"
                        Height "85%" ] ] (List.map (fun p -> displayPlayer p model dispatch) model.Players) ]

let displayCurrentCard model dispatch =
    div
        [ ClassName "card d-flex col"
          Id "active-card" ]
        [ div [ ClassName "card-body flex-wrap" ]
              [ button
                  [ OnClick(fun _ ->
                      do ChangeActiveCard |> dispatch
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

type HtmlAttr =
    | [<CompiledName("aria-valuenow")>] AriaValueNow of string
    | [<CompiledName("aria-valuemin")>] AriaValueMin of string
    | [<CompiledName("aria-valuemax")>] AriaValueMax of string
    interface IHTMLProp

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
          div [ ClassName "progress" ]
              [ div
                  [ ClassName "progress-bar"
                    Role "progressbar"
                    Style([ Width(sprintf "%d%%" ((model.Counter * 100 / model.Cards.Length))) ])
                    AriaValueNow(sprintf "%d" model.Counter)
                    AriaValueMin "0"
                    AriaValueMax(sprintf "%d" model.Cards.Length) ] [] ] ]

let displayActiveCard (card: RawCard) model dispatch =
    div [ ClassName "card p-2 m-1"
          Title card.text ] [ h5 [ ClassName "card-title h-100" ] [ str card.text ] ]

let activeCards (model: Model) dispatch =
    div
        [ ClassName "active-cards row mr-4 mt-3"
          Style [ Height "22%"
                  OverflowY "scroll" ] ]
        [ div [ ClassName "col d-flex flex-wrap" ] (List.map (fun card -> displayActiveCard card model dispatch) model.ActiveCards) ]

let view (model: Model) dispatch =
    div
        [ Ref(fun element ->
            if not (isNull element) then
                if model.InitialLoad then dispatch InitialLoad)
          ClassName "container-fluid h-100" ]
        [ div [ ClassName "row m-4" ] [ (displayInformationHeader model dispatch) ]
          div
              [ ClassName "row m-2"
                Style [ Height "65%" ] ]
              [ (displayCurrentCard model dispatch)
                (sidebar model dispatch) ]
          (activeCards model dispatch) ]

// App
Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run
