module Main exposing (..)

import Browser
import Html exposing (Html, br, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    Piece


type alias Piece =
    Tetromino


{-| <https://en.wikipedia.org/wiki/Tetromino>
-}
type Tetromino
    = I
    | O


type Bag
    = Set Tetromino


type Playfield
    = Int


init : Model
init =
    O



-- UPDATE


type Msg
    = NewPiece


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewPiece ->
            I



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ showPlayField
        , showPiece model
        , button [ onClick NewPiece ] [ text "New Piece." ]
        ]

showPlayField : Html Msg
showPlayField =
    div [ style "font-family" "monospace" ]
        [ text "``````"
        , br [] []
        , text "``````"
        , br [] []
        , text "``````"
        , br [] []
        , text "``````"
        , br [] []
        , text "``````"
        , br [] []
        , text "``````"
        ]


showPiece : Model -> Html Msg
showPiece model =
    case model of
        I ->
            div [] [ text "IIII" ]

        O ->
            div []
                [ text "OO"
                , br [] []
                , text "OO"
                ]
