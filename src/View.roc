module [renderTypeSelect, renderInputAppName, renderPlatformSelect, renderPackageSelect, renderSearch, renderConfirmation, renderSplash, renderBox]

import AsciiArt
import BoxStyle exposing [BoxStyle, border]
import Controller exposing [UserAction]
import Model exposing [Model]
import ansi.ANSI

## Render functions for each page
renderScreenPrompt = \text -> ANSI.drawText text { r: 1, c: 2, fg: Standard Cyan }
renderExitPrompt = \screen -> ANSI.drawText " Ctrl+C : QUIT " { r: 0, c: screen.width - 17, fg: Standard Red }
renderControlsPrompt = \text, screen -> ANSI.drawText text { r: screen.height - 1, c: 2, fg: Standard Cyan }
renderOuterBorder = \screen -> renderBox 0 0 screen.width screen.height (CustomBorder { tl: "╒", t: "═", tr: "╕" }) (Standard Cyan)

## Control prompts for each user action
controlPromptsDict : Dict UserAction Str
controlPromptsDict =
    Dict.empty {}
    |> Dict.insert SingleSelect "ENTER : SELECT"
    |> Dict.insert MultiSelect "SPACE : SELECT"
    |> Dict.insert MultiConfirm "ENTER : CONFIRM"
    |> Dict.insert TextSubmit "ENTER : CONFIRM"
    |> Dict.insert GoBack "BKSP : GO BACK"
    |> Dict.insert Search "S : SEARCH"
    |> Dict.insert ClearFilter "ESC : FULL LIST"
    |> Dict.insert SearchGo "ENTER : SEARCH"
    |> Dict.insert Cancel "ESC : CANCEL"
    |> Dict.insert Finish "ENTER : FINISH"
    |> Dict.insert CursorUp ""
    |> Dict.insert CursorDown ""
    |> Dict.insert TextInput ""
    |> Dict.insert TextBackspace ""
    |> Dict.insert Exit ""
    |> Dict.insert None ""
    |> Dict.insert Secret ""
    |> Dict.insert PrevPage "< PREV"
    |> Dict.insert NextPage "> NEXT"

## Shortened control prompts for smaller screens
controlPromptsShortDict : Dict UserAction Str
controlPromptsShortDict =
    Dict.empty {}
    |> Dict.insert SingleSelect "ENTER"
    |> Dict.insert MultiSelect "SPACE"
    |> Dict.insert MultiConfirm "ENTER"
    |> Dict.insert TextSubmit "ENTER"
    |> Dict.insert GoBack "BKSP"
    |> Dict.insert Search "S"
    |> Dict.insert ClearFilter "ESC"
    |> Dict.insert SearchGo "ENTER"
    |> Dict.insert Cancel "ESC"
    |> Dict.insert Finish "ENTER"
    |> Dict.insert CursorUp ""
    |> Dict.insert CursorDown ""
    |> Dict.insert TextInput ""
    |> Dict.insert TextBackspace ""
    |> Dict.insert Exit ""
    |> Dict.insert None ""
    |> Dict.insert Secret ""
    |> Dict.insert PrevPage "<"
    |> Dict.insert NextPage ">"

## Build string with all available controls
controlsPromptStr : Model -> Str
controlsPromptStr = \model ->
    actions = Controller.getActions model
    longStr = buildControlPromptStr actions controlPromptsDict
    promptLen = Num.toU16 (Str.countUtf8Bytes longStr)
    if promptLen <= model.screen.width - 6 && promptLen > 0 then
        " $(longStr) "
    else if promptLen > 0 then
        " $(buildControlPromptStr actions controlPromptsShortDict) "
    else
        ""

buildControlPromptStr : List UserAction, Dict UserAction Str -> Str
buildControlPromptStr = \actions, promptsDict ->
    actions
    |> List.map \action ->
        Dict.get promptsDict action |> Result.withDefault ""
    |> List.dropIf (\str -> Str.isEmpty str)
    |> Str.joinWith " | "

## Render a multi-line text with word wrapping
renderMultiLineText : List Str, { startCol : U16, startRow : U16, maxCol : U16, wrapCol : U16, wordDelim ? Str, fg ? ANSI.Color } -> List ANSI.DrawFn
renderMultiLineText = \words, { startCol, startRow, maxCol, wrapCol, wordDelim ? " ", fg ? Standard White } ->
    firstLineWidth = maxCol - startCol
    consecutiveWidths = maxCol - wrapCol
    delims = List.repeat wordDelim (if List.len words == 0 then 0 else List.len words - 1) |> List.append ""
    wordsWithDelims = List.map2 words delims \word, delim -> Str.concat word delim
    lineList =
        List.walk wordsWithDelims [] \lines, word ->
            when lines is
                [line] ->
                    if Num.toU16 (Str.countUtf8Bytes line + Str.countUtf8Bytes word) <= firstLineWidth then
                        [Str.concat line word]
                    else
                        [line, word]

                [.. as prevLines, line] ->
                    if Num.toU16 (Str.countUtf8Bytes line + Str.countUtf8Bytes word) <= consecutiveWidths then
                        List.concat prevLines [Str.concat line word]
                    else
                        List.concat prevLines [line, word]

                [] -> [word]
    List.mapWithIndex lineList \line, idx ->
        if idx == 0 then
            ANSI.drawText line { r: startRow, c: startCol, fg }
        else
            ANSI.drawText line { r: startRow + (Num.toU16 idx), c: wrapCol, fg }

renderTypeSelect : Model -> List ANSI.DrawFn
renderTypeSelect = \model ->
    List.join [
        [
            renderExitPrompt model.screen,
            renderControlsPrompt (controlsPromptStr model) model.screen,
        ],
        renderOuterBorder model.screen,
        [
            renderScreenPrompt "WHAT TO START?",
            ANSI.drawCursor { fg: Standard Magenta, char: ">" },
        ],
        renderMenu model,
    ]

## Generate the list of functions to draw the platform select page.
renderPlatformSelect : Model -> List ANSI.DrawFn
renderPlatformSelect = \model ->
    List.join [
        [
            renderExitPrompt model.screen,
            renderControlsPrompt (controlsPromptStr model) model.screen,
        ],
        renderOuterBorder model.screen,
        [
            renderScreenPrompt "SELECT A PLATFORM:",
            ANSI.drawCursor { fg: Standard Magenta, char: ">" },
        ],
        renderMenu model,
    ]

## Generate the list of functions to draw the package select page.
renderPackageSelect : Model -> List ANSI.DrawFn
renderPackageSelect = \model ->
    List.join [
        [
            renderExitPrompt model.screen,
            renderControlsPrompt (controlsPromptStr model) model.screen,
        ],
        renderOuterBorder model.screen,
        [
            renderScreenPrompt "SELECT 0+ PACKAGES:",
            ANSI.drawCursor { fg: Standard Magenta, char: ">" },
        ],
        renderMultipleChoiceMenu model,

    ]

## Generate the list of functions to draw the app name input page.
renderInputAppName : Model -> List ANSI.DrawFn
renderInputAppName = \model ->
    when model.state is
        InputAppName { nameBuffer } ->
            List.join [
                [
                    renderExitPrompt model.screen,
                    renderControlsPrompt (controlsPromptStr model) model.screen,
                ],
                renderOuterBorder model.screen,
                if List.len nameBuffer == 0 then [ANSI.drawText " (Leave blank for \"main\"):" { r: 1, c: 20, fg: Standard Cyan }] else [],
                [
                    renderScreenPrompt "ENTER THE APP NAME:",
                    ANSI.drawCursor { fg: Standard Magenta, char: ">" },
                    ANSI.drawText (nameBuffer |> Str.fromUtf8 |> Result.withDefault "") { r: model.menuRow, c: 4, fg: Standard White },

                ],
            ]

        _ -> []

## Generate the list of functions to draw the search page.
renderSearch : Model -> List ANSI.DrawFn
renderSearch = \model ->
    when model.state is
        Search { sender, searchBuffer } ->
            searchPrompt = if sender == Package then "SEARCH FOR A PACKAGE:" else "SEARCH FOR A PLATFORM:"
            List.join [
                [
                    renderExitPrompt model.screen,
                    renderControlsPrompt (controlsPromptStr model) model.screen,
                ],
                renderOuterBorder model.screen,
                [
                    renderScreenPrompt searchPrompt,
                    ANSI.drawCursor { fg: Standard Magenta, char: ">" },
                    ANSI.drawText (searchBuffer |> Str.fromUtf8 |> Result.withDefault "") { r: model.menuRow, c: 4, fg: Standard White },
                ],
            ]

        _ -> []

## Generate the list of functions to draw the confirmation page.
renderConfirmation : Model -> List ANSI.DrawFn
renderConfirmation = \model ->
    when model.state is
        Confirmation { config } ->
            List.join [
                [
                    renderExitPrompt model.screen,
                    renderControlsPrompt (controlsPromptStr model) model.screen,
                ],
                renderOuterBorder model.screen,
                (
                    if config.type == App then
                        [
                            renderScreenPrompt "APP CONFIGURATION:",
                            ANSI.drawText "App name:" { r: model.menuRow, c: 2, fg: Standard Magenta },
                            ANSI.drawText config.fileName { r: model.menuRow, c: 12, fg: Standard White },
                            ANSI.drawText "Platform:" { r: model.menuRow + 1, c: 2, fg: Standard Magenta },
                            ANSI.drawText config.platform { r: model.menuRow + 1, c: 12, fg: Standard White },
                            ANSI.drawText "Packages:" { r: model.menuRow + 2, c: 2, fg: Standard Magenta },
                        ]
                    else
                        [
                            renderScreenPrompt "PACKAGE CONFIGURATION:",
                            ANSI.drawText "Packages:" { r: model.menuRow, c: 2, fg: Standard Magenta },
                        ]
                ),
                renderMultiLineText config.packages {
                    startCol: 12,
                    startRow: if config.type == App then (model.menuRow + 2) else model.menuRow,
                    maxCol: (model.screen.width - 1),
                    wrapCol: 2,
                    wordDelim: ", ",
                    fg: Standard White,
                },
            ]

        _ -> []

## Generate the list of functions to draw a box.
renderBox : U16, U16, U16, U16, BoxStyle, ANSI.Color -> List ANSI.DrawFn
renderBox = \col, row, width, height, style, color -> [
    ANSI.drawHLine { r: row, c: col, len: 1, char: border TopLeft style, fg: color },
    ANSI.drawHLine { r: row, c: col + 1, len: width - 2, char: border Top style, fg: color },
    ANSI.drawHLine { r: row, c: col + width - 1, len: 1, char: border TopRight style, fg: color },
    ANSI.drawVLine { r: row + 1, c: col, len: height - 2, char: border Left style, fg: color },
    ANSI.drawVLine { r: row + 1, c: col + width - 1, len: height - 2, char: border Right style, fg: color },
    ANSI.drawHLine { r: row + height - 1, c: col, len: 1, char: border BotLeft style, fg: color },
    ANSI.drawHLine { r: row + height - 1, c: col + 1, len: width - 2, char: border Bot style, fg: color },
    ANSI.drawHLine { r: row + height - 1, c: col + width - 1, len: 1, char: border BotRight style, fg: color },
]

## Generate the list of functions to draw a single select menu.
renderMenu : Model -> List ANSI.DrawFn
renderMenu = \model ->
    List.mapWithIndex model.menu \item, idx ->
        row = Num.toU16 idx + model.menuRow
        if model.cursor.row == row then
            ANSI.drawText "> $(item)" { r: row, c: 2, fg: Standard Magenta }
        else
            ANSI.drawText "- $(item)" { r: row, c: 2, fg: Default }

## Generate the list of functions to draw a multiple choice menu.
renderMultipleChoiceMenu : Model -> List ANSI.DrawFn
renderMultipleChoiceMenu = \model ->
    isSelected = \item -> List.contains model.selected item
    checkedItems = List.map model.menu \item -> if isSelected item then "[X] $(item)" else "[ ] $(item)"
    List.mapWithIndex checkedItems \item, idx ->
        row = Num.toU16 idx + model.menuRow
        if model.cursor.row == row then
            ANSI.drawText "> $(item)" { r: row, c: 2, fg: Standard Magenta }
        else
            ANSI.drawText "- $(item)" { r: row, c: 2, fg: Default }

renderSplash : Model -> List ANSI.DrawFn
renderSplash = \model ->
    List.join [
        [
            renderExitPrompt model.screen,
            renderControlsPrompt (controlsPromptStr model) model.screen,
        ],
        renderOuterBorder model.screen,
        renderSplashBySize model.screen,
    ]

renderSplashBySize : ANSI.ScreenSize -> List ANSI.DrawFn
renderSplashBySize = \screen ->
    art = chooseSplashArt screen
    startRow = (screen.height - art.height) // 2
    startCol = (screen.width - art.width) // 2
    List.join [
        renderArtAccent art screen,
        renderAsciiArt art startRow startCol,
    ]

renderAsciiArt : AsciiArt.Art, U16, U16 -> List ANSI.DrawFn
renderAsciiArt = \art, startRow, startCol ->
    List.map art.art \elem ->
        ANSI.drawText elem.text { r: startRow + elem.r, c: startCol + elem.c, fg: elem.color }

chooseSplashArt : ANSI.ScreenSize -> AsciiArt.Art
chooseSplashArt = \screen ->
    if
        (screen.height >= (AsciiArt.rocLargeColored.height + 2))
        && (screen.width >= (AsciiArt.rocLargeColored.width + 2))
    then
        AsciiArt.rocLargeColored
    else if
        (screen.height >= (AsciiArt.rocSmallColored.height + 2))
        && (screen.width >= (AsciiArt.rocSmallColored.width + 2))
    then
        AsciiArt.rocSmallColored
    else
        AsciiArt.rocStartColored

renderArtAccent : AsciiArt.Art, ANSI.ScreenSize -> List ANSI.DrawFn
renderArtAccent = \art, screen ->
    startRow = (screen.height - art.height) // 2
    startCol = (screen.width - art.width) // 2
    if art == AsciiArt.rocLargeColored then
        List.mapWithIndex AsciiArt.rocStart \line, idx ->
            ANSI.drawText line { r: startRow + 30 + Num.toU16 idx, c: startCol + 50, fg: Standard Cyan }
    else if art == AsciiArt.rocSmallColored then
        [
            ANSI.drawText "roc start" { r: startRow + 11, c: startCol + 16, fg: Standard Cyan },
            ANSI.drawText "quick start cli" { r: startRow + 12, c: startCol + 16, fg: Standard Cyan },
        ]
    else
        [ANSI.drawText " quick start cli" { r: startRow + 5, c: startCol, fg: Standard Cyan }]
