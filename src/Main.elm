module Main exposing (..)

import Browser
import Html exposing (Html, br, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random



-- MAIN


main =
    Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }



-- MODEL


type alias Model =
    ( Piece, Playfield )


type alias Piece =
    Tetromino


{-| <https://en.wikipedia.org/wiki/Tetromino>
-}
type Tetromino
    = I
    | O


type Bag
    = Set Tetromino


type alias Playfield =
    List (List String)


init : () -> ( Model, Cmd Msg )
init _ =
    ( ( O, emptyPlayfield ), Cmd.none )



-- UPDATE


type Msg
    = ActivePiece Piece
    | NewPiece


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ( piece, playfield ) =
    case msg of
        ActivePiece piece_ ->
            ( ( piece_, playfield ), Cmd.none )

        NewPiece ->
            ( ( piece, playfield ), Random.generate ActivePiece randomPiece )



-- VIEW


view : Model -> Html Msg
view ( piece, playfield ) =
    div []
        [ showPlayfield playfield
        , button [ onClick NewPiece ] [ text "New Piece." ]
        , showPiece piece
        ]


emptyPlayfield : Playfield
emptyPlayfield =
    [ [ "`", "`", "`", "`", "`", "`" ]
    , [ "`", "`", "`", "`", "`", "`" ]
    , [ "`", "`", "`", "`", "`", "`" ]
    , [ "`", "`", "`", "`", "`", "`" ]
    , [ "`", "`", "`", "`", "`", "`" ]
    , [ "`", "`", "`", "`", "`", "`" ]
    ]


showPlayfield : Playfield -> Html Msg
showPlayfield playfield =
    div [ style "font-family" "monospace" ]
        (List.map showRow playfield)


showRow : List String -> Html Msg
showRow row =
    div [] [ text (String.join "" row) ]


showPiece : Piece -> Html Msg
showPiece piece =
    case piece of
        I ->
            div [] [ text "IIII" ]

        O ->
            div []
                [ text "OO"
                , br [] []
                , text "OO"
                ]


drawPiece : List String -> List (Html Msg)
drawPiece pieceStrings =
    List.map (\str -> text str) pieceStrings


randomPiece : Random.Generator Piece
randomPiece =
    Random.uniform I [ O ]


textPiece : Piece -> List (List String)
textPiece piece =
    case piece of
        I ->
            [ [ "I", "I", "I", "I" ]
            ]

        O ->
            [ [ "O", "O" ]
            , [ "O", "O" ]
            ]


addPieceToPlayfield : Piece -> Playfield -> Playfield
addPieceToPlayfield piece playfield =
    [ [ "`", "I", "I", "I", "I", "`" ]
    , [ "`", "`", "`", "`", "`", "`" ]
    , [ "`", "`", "`", "`", "`", "`" ]
    , [ "`", "`", "`", "`", "`", "`" ]
    , [ "`", "`", "`", "`", "`", "`" ]
    , [ "`", "`", "`", "`", "`", "`" ]
    ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
