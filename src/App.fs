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
      CurrentCard = None
      Cards = List.empty
      CurrentPlayer = None
      Counter = 0
      DisplayPlayerNameDuplicateError = false
      InitialLoad = true }, Cmd.Empty

let playerComp p1 p2 =
    p1.Name = p2.Name

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

// UPDATE

let getNextCard (cards: RawCard list) =
    let cards = List.filter (fun card -> card.count > 0) cards
    if cards.Length = 0 then
        None
    else
        let card = cards.Item(System.Random().Next() % cards.Length)
        let replacement_text = (Seq.map (fun w -> if w = "{int}" then (sprintf "%d" ((System.Random().Next()) % 9 + 2)) else w) (card.text.Split ' ')) |> String.concat " "
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
        let card = getNextCard model.Cards
        { model with
              CurrentCard = card
              Cards = decreaseCardCount card model.Cards }, if card.IsSome then Cmd.ofSub (fun dispatch -> dispatch IncrementCounter) else Cmd.Empty
    | ChangeActivePlayer ->
        { model with CurrentPlayer = findNextActivePlayer (List.filter (fun p -> p.Active || (match model.CurrentPlayer with
                                                                                                | Some cp -> cp.Name = p.Name
                                                                                                | None -> false )) model.Players) model },
        Cmd.Empty
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
    div [ ClassName "card" ]
        [ div
            [ Id
                (if model.CurrentPlayer.IsSome && model.CurrentPlayer.Value.Name = player.Name
                 then "player-current"
                 else "")
              ClassName("card-body " + if player.Active then "player-active" else "player-inactive") ]
              [ h5 [ ClassName "card-title text-center" ] [ str player.Name ]
                div [ ClassName "d-flex justify-content-around" ]
                    [ button
                        [ ClassName "card-text"
                          OnClick(fun _ -> TogglePlayerActivity player |> dispatch) ] [ str "Toggle" ]
                      button
                          [ ClassName "card-text"
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
    div [ ClassName "col-md-2 bg-light sidebar" ]
        [ div []
              [ input
                  [ Name "add-player-field"
                    Id "add-player-field"
                    OnKeyDown(fun x ->
                        if x.keyCode = 13. then (addPlayerFunction model dispatch)) ]
                button
                    [ ClassName "align-self-center"
                      OnClick(fun _ -> addPlayerFunction model dispatch) ] [ str "Add player" ] ]
          hr []
          div [ ClassName "card" ] (List.map (fun p -> displayPlayer p model dispatch) model.Players) ]

let view (model: Model) dispatch =
    div [ Ref (fun element -> if not (isNull element) then if model.InitialLoad then dispatch InitialLoad) ]
        [ (sidebar model dispatch)
          p
              [ Id "active-player-header"
                ClassName "text-center" ]
              [ str
                  ((match model.CurrentPlayer with
                    | Some player -> player.Name
                    | None -> "No active player")
                   + (sprintf " | %d" model.Counter)) ]
          div
              [ ClassName "card"
                Id "card" ]
              [ button
                  [ OnClick(fun _ ->
                      do ChangeActiveCard |> dispatch
                         ChangeActivePlayer |> dispatch)
                    ClassName "card-body card-title"
                    Disabled(model.CurrentCard.IsNone && model.Counter > 0) ]
                    [ str
                        (match model.CurrentCard with
                         | Some (card) -> card.text
                         | None ->
                             if model.Counter = 0 then "Click to start" else "No cards left") ] ] ]

// App
Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run
