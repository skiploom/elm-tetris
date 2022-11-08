module Main exposing (..)

import Browser
import Html exposing (Html, br, button, div, text)
import Html.Events exposing (onClick)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    Tetromino


type Tetromino
    = I
    | O


init : Model
init =
    I



-- UPDATE


type Msg
    = NewPiece


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewPiece ->
            O



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ showPiece model
        , button [ onClick NewPiece ] [ text "New Piece." ]
        ]


showPiece : Model -> Html Msg
showPiece model =
    case model of
        I ->
            div [] [ text "■■■■" ]

        O ->
            div []
                [ text "■■"
                , br [] []
                , text "■■"
                ]
