module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

import SimpleEdit

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


------------------------------------------------------------
-- MODEL
------------------------------------------------------------

type alias Model =
    { editModel : SimpleEdit.Model }

init : (Model, Cmd Msg)
init =
    ( { editModel = SimpleEdit.initialModel}
    , Cmd.none )


------------------------------------------------------------
-- UPDATE
------------------------------------------------------------

type Msg
    = SimpleEditMsg SimpleEdit.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SimpleEditMsg subMsg ->
            let
                (updatedEditModel, cmd) = SimpleEdit.update subMsg model.editModel
            in
                ( {model | editModel = updatedEditModel}
                , Cmd.map SimpleEditMsg cmd)

------------------------------------------------------------
-- VIEW
------------------------------------------------------------

view : Model -> Html Msg
view model =
    div [ style [("margin", "0"), ("padding", "0"), ("width", "100%"), ("height", "100%")] ]
        [ h1 [style [("margin", "0"),("box-sizing", "border-box"), ("height", "100px")]] [text "Editor Test"]
        , div [style [("margin", "0"), ("padding", "0"), ("width", "100%"), ("height", "calc( 100% - 100px )")] ]
            [ Html.map SimpleEditMsg (SimpleEdit.view model.editModel)
            ]
        ]

------------------------------------------------------------
-- SUBSCRIPTIONS
------------------------------------------------------------

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
