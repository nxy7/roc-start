module [
    Model,
    init,
    isNotFirstPage,
    isNotLastPage,
    getHighlightedIndex,
    getHighlightedItem,
    getSelectedItems,
    menuIsFiltered,
    getBuffer,
    getConfig,
]

import ansi.Core

Model : {
    screen : Core.ScreenSize,
    cursor : Core.Position,
    menuRow : I32,
    pageFirstItem : U64,
    menu : List Str,
    fullMenu : List Str,
    selected : List Str,
    inputs : List Core.Input,
    packageList : List Str,
    platformList : List Str,
    state : [
        TypeSelect { config : Configuration },
        InputAppName { nameBuffer : List U8, config : Configuration },
        Search { searchBuffer : List U8, config : Configuration, sender : [Platform, Package] },
        PlatformSelect { config : Configuration },
        PackageSelect { config : Configuration },
        Confirmation { config : Configuration },
        Finished { config : Configuration },
        Splash { config : Configuration },
        UserExited,
    ],
}

Configuration : {
    type : [App, Pkg],
    fileName : Str,
    platform : Str,
    packages : List Str,
}

emptyAppConfig = { fileName: "", platform: "", packages: [], type: App }

## Initialize the model
init : List Str, List Str -> Model
init = \platformList, packageList -> {
    screen: { width: 0, height: 0 },
    cursor: { row: 2, col: 2 },
    menuRow: 2,
    pageFirstItem: 0,
    menu: ["App", "Package"],
    fullMenu: ["App", "Package"],
    platformList,
    packageList,
    selected: [],
    inputs: List.withCapacity 1000,
    state: TypeSelect { config: emptyAppConfig },
}

## Check if the current page is not the first page
isNotFirstPage : Model -> Bool
isNotFirstPage = \model -> model.pageFirstItem > 0

## Check if the current page is not the last page
isNotLastPage : Model -> Bool
isNotLastPage = \model ->
    maxItems = model.screen.height - (model.menuRow + 1) |> Num.toU64
    model.pageFirstItem + maxItems < List.len model.fullMenu

## Get the index of the highlighted item
getHighlightedIndex : Model -> U64
getHighlightedIndex = \model -> Num.toU64 model.cursor.row - Num.toU64 model.menuRow

## Get the highlighted item
getHighlightedItem : Model -> Str
getHighlightedItem = \model -> List.get model.menu (getHighlightedIndex model) |> Result.withDefault ""

## Get the selected items in a multi-select menu
getSelectedItems : Model -> List Str
getSelectedItems = \model -> model.selected

## Check if the menu is currently filtered
menuIsFiltered : Model -> Bool
menuIsFiltered = \model ->
    when model.state is
        PlatformSelect _ -> List.len model.fullMenu < List.len model.platformList
        PackageSelect _ -> List.len model.fullMenu < List.len model.packageList
        _ -> Bool.false

getBuffer : Model -> Result (List U8) [InvalidState]
getBuffer = \model ->
    when model.state is
        InputAppName { nameBuffer } -> Ok nameBuffer
        Search { searchBuffer } -> Ok searchBuffer
        _ -> Err InvalidState

getConfig : Model -> Result Configuration [InvalidState]
getConfig = \model ->
    when model.state is
        TypeSelect { config } -> Ok config
        InputAppName { config } -> Ok config
        Search { config } -> Ok config
        PlatformSelect { config } -> Ok config
        PackageSelect { config } -> Ok config
        Confirmation { config } -> Ok config
        Finished { config } -> Ok config
        Splash { config } -> Ok config
        UserExited -> Err InvalidState
