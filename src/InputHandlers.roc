module [
    default,
    typeSelect,
    platformSelect,
    packageSelect,
    search,
    inputAppName,
    confirmation,
    splash,
]

import ansi.Core
import cli.Task exposing [Task]

import Model exposing [Model]
import Controller

## Default input handler which ensures that the program can always be exited.
## This ensures that even if you forget to handle input for a state, or end up
## in a state that doesn't have an input handler, the program can still be exited.
default : Model, Core.Input -> Task [Step Model, Done Model] _
default = \model, input ->
    action =
        when input is
            CtrlC -> Exit
            _ -> None
    Task.ok (Controller.applyAction { model, action })

## The input handler for the TypeSelect state.
typeSelect : Model, Core.Input -> Task [Step Model, Done Model] _
typeSelect = \model, input ->
    action =
        when input is
            CtrlC -> Exit
            KeyPress Enter -> SingleSelect
            KeyPress Up -> CursorUp
            KeyPress Down -> CursorDown
            KeyPress Right -> NextPage
            KeyPress GreaterThanSign -> NextPage
            KeyPress FullStop -> NextPage
            KeyPress Left -> PrevPage
            KeyPress LessThanSign -> PrevPage
            KeyPress Comma -> PrevPage
            KeyPress GraveAccent -> Secret
            _ -> None
    Task.ok (Controller.applyAction { model, action })

## The input handler for the PlatformSelect state.
platformSelect : Model, Core.Input -> Task [Step Model, Done Model] _
platformSelect = \model, input ->
    action =
        when input is
            CtrlC -> Exit
            KeyPress LowerS -> Search
            KeyPress UpperS -> Search
            KeyPress Enter -> SingleSelect
            KeyPress Up -> CursorUp
            KeyPress Down -> CursorDown
            KeyPress Delete -> GoBack
            KeyPress Escape -> ClearFilter
            KeyPress Right -> NextPage
            KeyPress GreaterThanSign -> NextPage
            KeyPress FullStop -> NextPage
            KeyPress Left -> PrevPage
            KeyPress LessThanSign -> PrevPage
            KeyPress Comma -> PrevPage
            _ -> None
    Task.ok (Controller.applyAction { model, action })

## The input handler for the PackageSelect state.
packageSelect : Model, Core.Input -> Task [Step Model, Done Model] _
packageSelect = \model, input ->
    action =
        when input is
            CtrlC -> Exit
            KeyPress LowerS -> Search
            KeyPress UpperS -> Search
            KeyPress Enter -> MultiConfirm
            KeyPress Space -> MultiSelect
            KeyPress Up -> CursorUp
            KeyPress Down -> CursorDown
            KeyPress Delete -> GoBack
            KeyPress Escape -> ClearFilter
            KeyPress Right -> NextPage
            KeyPress GreaterThanSign -> NextPage
            KeyPress FullStop -> NextPage
            KeyPress Left -> PrevPage
            KeyPress LessThanSign -> PrevPage
            KeyPress Comma -> PrevPage
            _ -> None
    Task.ok (Controller.applyAction { model, action })

## The input handler for the Search state.
search : Model, Core.Input -> Task [Step Model, Done Model] _
search = \model, input ->
    (action, keyPress) =
        when input is
            CtrlC -> (Exit, None)
            KeyPress Enter -> (SearchGo, None)
            KeyPress Escape -> (Cancel, None)
            KeyPress Delete -> (TextBackspace, None)
            KeyPress key -> (TextInput, KeyPress key)
            _ -> (None, None)
    Task.ok (Controller.applyAction { model, action, keyPress })

## The input handler for the InputAppName state.
inputAppName : Model, Core.Input -> Task [Step Model, Done Model] _
inputAppName = \model, input ->
    bufferLen =
        when model.state is
            InputAppName { nameBuffer } -> List.len nameBuffer
            _ -> 0
    (action, keyPress) =
        when input is
            CtrlC -> (Exit, None)
            KeyPress Enter -> (TextSubmit, None)
            KeyPress Delete -> if bufferLen == 0 then (GoBack, None) else (TextBackspace, None)
            KeyPress key -> (TextInput, KeyPress key)
            _ -> (None, None)
    Task.ok (Controller.applyAction { model, action, keyPress })

## The input handler for the Confirmation state.
confirmation : Model, Core.Input -> Task [Step Model, Done Model] _
confirmation = \model, input ->
    action =
        when input is
            CtrlC -> Exit
            KeyPress Enter -> Finish
            KeyPress Delete -> GoBack
            _ -> None
    Task.ok (Controller.applyAction { model, action })

## The input handler for the Splash state.
splash : Model, Core.Input -> Task [Step Model, Done Model] _
splash = \model, input ->
    action =
        when input is
            CtrlC -> Exit
            KeyPress Delete -> GoBack
            _ -> None
    Task.ok (Controller.applyAction { model, action })
