module Helper

open Fable.Core
open Fable.React
open Fable.React.Props

let unwrapMapOrDefault (opt: 'b option) (m: 'b -> 't) (def: 't) =
    if opt.IsSome then m opt.Value else def

let unwrapOr (opt: 'b option) (def: 'b): 'b =
    match opt with
    | Some value -> value
    | None -> def

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

let joinHtmlElements (sep: ReactElement) (l: ReactElement list) =
    Seq.ofList l
    |> Seq.fold (fun acc y ->
        if Seq.isEmpty acc
        then seq { y }
        else Seq.append acc (Seq.ofList [ sep; y ])) Seq.empty
    |> List.ofSeq

let play id =
    ((Browser.Dom.window.document.getElementById id) :?> Browser.Types.HTMLMediaElement).play()

[<Emit("$0.currentTime = $1")>]
let assignCurrentTime element value = jsNative

let stop id =
    ((Browser.Dom.window.document.getElementById id) :?> Browser.Types.HTMLMediaElement).pause()
    assignCurrentTime ((Browser.Dom.window.document.getElementById id) :?> Browser.Types.HTMLMediaElement) 0.0

let isActiveCard (card: Card.Type option) =
    card.IsSome && (card.Value.Rounds <> 0 || card.Value.Uses <> 0)