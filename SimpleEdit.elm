module SimpleEdit exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

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
    { lines = []
    , cursor = { line = 0
               , column = 0}
    }


------------------------------------------------------------
-- UPDATE
------------------------------------------------------------

type Msg
    = EditorInput String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        EditorInput content ->
            ( {model | lines = String.lines content}
            , Cmd.none)


------------------------------------------------------------
-- VIEW
------------------------------------------------------------

view : Model -> Html Msg
view model =
    div [ class "editor_frame"
        , style [("margin", "0"), ("padding", "0"), ("width", "100%"), ("height", "100%")]
        ]
        [ textarea [ class "editor_textarea"
                   , style [("opacity", "0.2")] -- debugように薄っすらと表示
                   , onInput EditorInput] []
                   -- wrap off , autocorrect off, autocapitalize off, specllcheck false
        , div [class "editor_screen"]
            (List.indexedMap (λ ln x -> div [class "editor_line"]
                                   [ div [class "editor_num_col"] [ pre [] [text (toString (ln + 1))] ]
                                   , div [class "editor_src_col"] [ pre [] [text x ] ]
                                   ]
                             ) model.lines)
        ]


