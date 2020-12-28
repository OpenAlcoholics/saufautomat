module View

open Fable.React
open Fable.React.Props
open Helper
open Model
open Resources

let modal (id: string) bodyContent footerContent =
    div [ ClassName "modal fade"
          Id id
          TabIndex -1
          Role "dialog" ] [
        div [ ClassName "modal-dialog"
              Role "document" ] [
            div [ ClassName "modal-content" ] [
                div [ ClassName "modal-body" ] [
                    div [ ClassName "form-group container" ] bodyContent
                ]

                div [ ClassName "modal-footer" ] footerContent
            ]
        ]
    ]

let modalButton (id: string) className disabled model resourceKey (extras: IHTMLProp list) =
    let id = match (id.StartsWith "#") with
             | true -> id
             | false -> "#" + id

    button ([ ClassName className
              Disabled disabled
              DataToggle "modal"
              DataTarget id ] @ extras) [
        str (getKey (model.Settings.Language) resourceKey)
    ]

let label hfor (model: Model) resourceKey =
    label [ HtmlFor hfor
            ClassName "col align-self-center" ] [
        str (getKey (model.Settings.Language) resourceKey)
    ]

let input name id inputtype value placeholder pattern (className: string option) (extras: IHTMLProp list) =
    let args: IHTMLProp list =
        [ Name name
          Id id
          ClassName
              (match className with
               | Some value -> value
               | None -> "m-1 w-100 col")
          InputType inputtype
          Placeholder placeholder
          DefaultValue value
          Pattern pattern ]
        @ extras

    input args
