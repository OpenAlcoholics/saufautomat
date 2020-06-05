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
      DisplayPlayerNameDuplicateError: bool }

type Msg =
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
        let url = "https://raw.githubusercontent.com/torbencarstens/drinking-game-cards/develop/tasks_is.json"
        let! res = Fetch.get (url)
        AddCards res |> dispatch
    }

let init (): Model * Cmd<Msg> =
    { Players = List.empty
      CurrentCard = None
      Cards = getCards |> unbox
      CurrentPlayer = None
      Counter = 0
      DisplayPlayerNameDuplicateError = false }, Cmd.Empty

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

let getNextCard cards =
    List.filter (fun c -> c.count > 0) cards

let update (msg: Msg) (model: Model) =
    match msg with
    | ChangeActiveCard ->
        if model.Cards.Length = 0 then
            model, Cmd.ofSub (fun dispatch -> getCards dispatch |> Promise.start)
        else
            { model with CurrentCard = Some(model.Cards.Item((System.Random().Next() % model.Cards.Length))) },  // TODO: Actually decrement count and use it
            Cmd.ofSub (fun dispatch -> dispatch IncrementCounter)
    | ChangeActivePlayer ->
        { model with CurrentPlayer = findNextActivePlayer (List.filter (fun p -> p.Active) model.Players) model },
        Cmd.Empty
    | IncrementCounter ->
        { model with Counter = model.Counter + 1 }, Cmd.Empty
    | AddCards cards ->
        { model with Cards = cards }, Cmd.ofSub (fun dispatch -> dispatch ChangeActiveCard)
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
    (ChangeActivePlayer |> dispatch)
    HidePlayerNameDuplicate |> ignore
    true

// TODO: check that there are no duplicate names
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
    div []
        [ (sidebar model dispatch)
          p
              [ Id "active-player-header"
                ClassName "text-center" ]
              [ str
                  ((match model.CurrentPlayer with
                   | Some player -> player.Name
                   | None -> "Kein aktiver Spieler") + (sprintf " | %d" model.Counter) ) ]
          div
              [ ClassName "card"
                Id "card" ]
              [ button
                  [ OnClick(fun _ ->
                      do ChangeActiveCard |> dispatch
                         ChangeActivePlayer |> dispatch)
                    ClassName "card-body card-title" ]
                    [ str
                        (match model.CurrentCard with
                         | Some (card) ->
                             (card.text.Replace("{int}", (sprintf "%d" ((System.Random().Next()) % 9 + 2))))
                         | None -> "Click to start") ] ] ]

// App
Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
|> Program.withConsoleTrace
|> Program.run
