module SimpleEdit exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Json

import Char exposing (..)
import String exposing (..)
import List exposing (..)

------------------------------------------------------------
-- Model
------------------------------------------------------------

type alias Cursor =
    { line : Int
    , column : Int
    }

type alias Model =
    { lines : List String
    , cursor : Cursor
    }

initialModel : Model
initialModel =
    { lines = [""]
    , cursor = { line = 0
               , column = 0}
    }


------------------------------------------------------------
-- UPDATE
------------------------------------------------------------

type Msg
      = KeyEvent Int
--    = EditorInput String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        KeyEvent code ->
            keyInput code model


-- Keyboard Event

keyInput : Int -> Model -> (Model, Cmd Msg)
keyInput code model =
    case code of
        37 -> -- '←'
            (moveLeft model, Cmd.none)
        38 -> -- '↑'
            (moveUp model, Cmd.none)
        39 -> -- '→'
            (moveRight model, Cmd.none)
        40 -> -- '↓'
            (moveDown model, Cmd.none)
        13 -> -- Enter
            (newLine model, Cmd.none)
        8 ->
            (backspace model, Cmd.none)
        _ ->
            (insertString (codeToString code) model, Cmd.none)

insertString : String -> Model -> Model
insertString str model =
    let
        cur = model.cursor
        len = String.length str
        line = getLine cur.line model.lines
    in
        {model | lines =
             if cur.line == 0
             then 
                 [(String.left cur.column line) ++ str ++ (String.dropLeft cur.column line)]
             ++ (drop (cur.line + 1) model.lines)
             else
                 (take (cur.line) model.lines)
             ++ [(String.left cur.column line) ++ str ++ (String.dropLeft cur.column line)]
             ++ (drop (cur.line + 1)model.lines)
        , cursor = {cur| column = cur.column + len}
        }

newLine : Model -> Model
newLine model =
    let
        line = getLine model.cursor.line model.lines
        cur = model.cursor
    in
        {model | lines =
             if cur.line == 0 then
                 [(String.left cur.column line)]
                 ++ [(String.dropLeft cur.column line)]
                 ++ (drop (cur.line + 1) model.lines)
             else
                 (take (cur.line) model.lines)
                 ++ [(String.left cur.column line)]
                 ++ [(String.dropLeft cur.column line)]
                 ++ (drop (cur.line + 1)model.lines)
        , cursor = {cur| line = cur.line + 1, column = 0}
        }

backspace : Model -> Model
backspace model =
    let
        line = getLine model.cursor.line model.lines
        cur = model.cursor
    in
        if cur.line == 0 then
            if cur.column == 0 then
                model
            else
                {model|
                     lines = [(String.left (cur.column - 1) line) ++ (String.dropLeft cur.column line)]
                             ++ (drop (cur.line + 1) model.lines)
                , cursor = {cur| column = cur.column - 1}
                }
        else
            if cur.column == 0 then
                let
                    prev_line = getLine (cur.line - 1) model.lines
                in
                    {model|
                         lines = (take (cur.line - 1) model.lines)
                               ++ [prev_line ++ line]
                               ++ (drop (cur.line + 1)model.lines)
                         , cursor = {cur| line = cur.line - 1, column = String.length prev_line}
                    }
            else
                {model |
                     lines =
                         (take (cur.line) model.lines)
                         ++ [(String.left (cur.column - 1)line)++ (String.dropLeft cur.column line)]
                         ++ (drop (cur.line + 1)model.lines)
                     , cursor = {cur|  column = cur.column - 1}
                }

moveLeft : Model -> Model
moveLeft model =
    let
        cur = model.cursor
    in
        case cur.column of
            0 ->
                if cur.line == 0 then
                    model
                else
                    {model | cursor = {cur| line = cur.line - 1
                                          , column = String.length (getLine (cur.line - 1) model.lines)}}
            _ ->
                {model | cursor = {cur| column = cur.column - 1}}

moveRight : Model -> Model
moveRight model =
    let
        line = getLine model.cursor.line model.lines
        cur = model.cursor
        last = (String.length line)
    in
        if cur.column == last then
            if cur.line == (List.length model.lines) - 1then 
                model
            else
                {model | cursor = {cur| line = cur.line + 1
                                      , column = 0}}
        else
            {model | cursor = {cur| column = cur.column + 1}}

moveUp : Model -> Model
moveUp model =
    let
        cur = model.cursor
    in
        if cur.line == 0 then 
            model
        else
            {model| cursor = {cur|line = cur.line - 1}}

moveDown : Model -> Model
moveDown model =
    let
        cur = model.cursor
    in
        if cur.line == (List.length model.lines) - 1 then 
            model
        else
            {model| cursor = {cur|line = cur.line + 1}}



getLine : Int -> List String -> String
getLine linum lines =
    Maybe.withDefault "" (head (drop linum lines))


codeToString : Int -> String
codeToString code =
    fromChar (fromCode code)

------------------------------------------------------------
-- VIEW
------------------------------------------------------------

view : Model -> Html Msg
view model =
    div [ class "editor_frame"
        , style [("margin", "0"), ("padding", "0"), ("width", "100%"), ("height", "100%")]
        ]
        [ textarea [ class "editor_textarea"
                   , style [("opacity", "0.0")] -- debugように薄っすらと表示
--                   , onInput EditorInput]
                   , onKeyUp KeyEvent]  []
                   -- wrap off , autocorrect off, autocapitalize off, specllcheck false
        , div [class "editor_screen"]
            (List.indexedMap (λ ln x -> div [class "editor_line"]
                                   [ div [class "editor_num_col"] [ pre [] [text (toString (ln + 1))] ]
                                   , div [class "editor_src_col"] [ pre []
                                                                        (if ln == model.cursor.line
                                                                         then cursorMarkup model.cursor.column x
                                                                         else [text x]
                                                                        ) ]
                                   ]
                             ) model.lines)
        ]

cursorMarkup : Int -> String -> List (Html Msg)
cursorMarkup cur str =
    if String.length str < cur then
        [text str]
    else
        [ span [class "cur_front"][text (String.left cur str)]
        , text (String.dropLeft cur str)
        ]

-- Helper

onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
    on "keydown" (Json.map tagger keyCode)
