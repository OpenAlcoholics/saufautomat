module Helper

open System.Text.RegularExpressions
open Browser
open Card
open Elmish
open Fable.Core
open Fable.React
open Fable.React.Props
open Model
open System
open Thoth.Fetch
open Thoth.Json

let unwrapMapOrDefault (opt: 'b option) (m: 'b -> 't) (def: 't) = if opt.IsSome then m opt.Value else def

let unwrapOr (opt: 'b option) (def: 'b): 'b =
    match opt with
    | Some value -> value
    | None -> def

let findCookieValue (name: string): string option =
    let kvArrToPair (kvArr: string []): string * string =
        match kvArr with
        | [| k; v |] -> (k, v)
        | _ -> ("", "")

    let rawCookies: string = Dom.document.cookie

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

let allowedLanguages = [ "de"; "en" ]

let joinHtmlElements (sep: ReactElement) (l: ReactElement list) =
    Seq.ofList l
    |> Seq.fold (fun acc y -> if Seq.isEmpty acc then seq { y } else Seq.append acc (Seq.ofList [ sep; y ])) Seq.empty
    |> List.ofSeq

let play id =
    ((Dom.window.document.getElementById id) :?> Browser.Types.HTMLMediaElement)
        .play()

[<Emit("$0[$1] = $2")>]
let assignElement element key value = jsNative

let assignCurrentTime element value =
    assignElement element "currentTime" value

let stop id =
    ((Dom.window.document.getElementById id) :?> Browser.Types.HTMLMediaElement)
        .pause()

    assignCurrentTime ((Dom.window.document.getElementById id) :?> Browser.Types.HTMLMediaElement) 0.0

let isActiveCard (card: Card.Type option) =
    card.IsSome
    && (card.Value.Rounds <> 0 || card.Value.Uses <> 0)

let stocv s =
    if s = "i18n" then CardsVersion.I18N else CardsVersion.V2

let cvtos cv =
    if cv = CardsVersion.I18N then "i18n" else "v2"

let getValueFromHtmlInput id def =
    match ((Dom.window.document.getElementById id) :?> Browser.Types.HTMLInputElement) with
    | null -> def
    | element -> match element.value with
                 | "" -> def
                 | value -> value

let getCheckedFromHtmlElement id =
    ((Dom.window.document.getElementById id) :?> Browser.Types.HTMLInputElement)
        .``checked``

let roundHasEnded model =
    model.RoundInformation.CardsToPlay <= 0
    && model.Players.Length > 0

let removeNodeById id =
    match (Dom.window.document.getElementById id) with
            | null -> ()
            | x -> x.remove()

let assignValueToHtmlInput id value =
    ((Dom.window.document.getElementById id) :?> Browser.Types.HTMLInputElement).value <- value

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

let explodeCards cards =
    (List.map (fun card ->
        ([ card ]
         |> Seq.collect (fun c -> List.replicate c.Count { c with Count = 1; OriginalCount = c.Count }))) cards)
    |> Seq.reduce Seq.append
    |> List.ofSeq

let getPlayerByIndex index (players: Player.Type list): Player.Type option =
    try
        Some(players.Item index)
    with _ -> None

let decreaseCount card cards =
    match card with
    | Some card -> List.map (fun c -> if c = card then { c with Count = c.Count - 1 } else c) cards
    | None -> cards

let getDistinctCount cards =
    (List.map (fun c -> c.Id) cards |> List.distinct)
        .Length

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

let getNextCard (cards: Card.Type list) model =
    let cards = filterCardsForTurn cards model

    if cards.Length = 0
    then None
    else Some(cards.Item(Random().Next() % cards.Length))

let rec findNextActivePlayer (playerList: Player.Type list) (model: Model) =
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

let generateUniqueId basestring (values: String list) isModal isId =
    sprintf
        "%s%s%s%s"
        (if isId then "#" else "")
        basestring
        (if isModal then "modal" else "")
        (values |> List.map (fun s -> s.Replace("-", "")) |> String.concat "")

let generateActiveCardId card (player: Player.Type option) isModal isId =
    generateUniqueId "activecardnote" [card.Id.ToString(); (unwrapMapOrDefault player (fun p -> p.GId) "")] isModal isId

let generatePlayerReassignmentId card isModal isId =
    generateUniqueId "cardplayerreassignment" [card.Id.ToString()] isModal isId

let sendReview (review: JsonValue) dispatch =
    promise {
        let! res = Fetch.tryPost<_, _> ("https://review.saufautom.at/add", review)
//        let! res = Fetch.tryPost<_, _> ("http://localhost:8000/add", review)

        FinishReview res |> dispatch
    }
let rec randomExclusive min max (unusable: int list) =
    let r =
        (Random().Next()) % (max - min + 1) + min

    let forceReturn =
        (max - min + 1) <= (List.length unusable)

    if List.exists ((=) r) unusable && not forceReturn
    then randomExclusive min max unusable
    else r

let int_replacement_regex = Regex("{int([^}]*)?}")

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

    { card with ReplacedText = replacement_text }
