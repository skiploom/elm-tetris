module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html, br, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random
import Time



-- MAIN


main =
    Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }



-- MODEL


type alias Model =
    { piece : Piece
    , playfield : Playfield
    , secondsElapsed : SecondsElapsed
    , activePieceMask : Playfield
    , currentPosition : Position
    }


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
    Array (Array String)


type alias SecondsElapsed =
    Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( { piece = O
      , playfield = emptyPlayfield
      , secondsElapsed = 0
      , activePieceMask = emptyPlayfield
      , currentPosition =
            { one = ( 0, 1 )
            , two = ( 0, 2 )
            , three = ( 1, 1 )
            , four = ( 1, 2 )
            }
      }
    , Cmd.none
    )


type alias Position =
    { one : ( Int, Int ), two : ( Int, Int ), three : ( Int, Int ), four : ( Int, Int ) }



-- UPDATE


type Msg
    = ActivePiece Piece
    | AddPiece
    | NewPiece
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ piece, playfield, secondsElapsed } as model) =
    case msg of
        ActivePiece piece_ ->
            ( { model | piece = piece_ }, Cmd.none )

        AddPiece ->
            ( { model | playfield = addPieceToPlayfield piece playfield secondsElapsed }, Cmd.none )

        NewPiece ->
            ( model, Random.generate ActivePiece randomPiece )

        Tick time ->
            ( { model | secondsElapsed = secondsElapsed + 1 }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view { piece, playfield } =
    div []
        [ showPlayfield playfield
        , button [ onClick AddPiece ] [ text "Add Piece." ]
        , button [ onClick NewPiece ] [ text "New Piece." ]
        , showPiece piece
        ]


emptyPlayfield : Playfield
emptyPlayfield =
    Array.fromList
        [ Array.fromList [ "`", "`", "`", "`", "`", "`" ]
        , Array.fromList [ "`", "`", "`", "`", "`", "`" ]
        , Array.fromList [ "`", "`", "`", "`", "`", "`" ]
        , Array.fromList [ "`", "`", "`", "`", "`", "`" ]
        , Array.fromList [ "`", "`", "`", "`", "`", "`" ]
        , Array.fromList [ "`", "`", "`", "`", "`", "`" ]
        ]


showPlayfield : Playfield -> Html Msg
showPlayfield playfield =
    div [ style "font-family" "monospace" ]
        (Array.toList (Array.map showRow playfield))


showRow : Array String -> Html Msg
showRow row =
    div [] [ text (String.join "" (Array.toList row)) ]


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


addPieceToPlayfield : Piece -> Playfield -> SecondsElapsed -> Playfield
addPieceToPlayfield piece playfield secondsElapsed =
    let
        row =
            if secondsElapsed > 5 then
                5

            else
                secondsElapsed

        activePieceMask =
            Array.fromList [ Array.fromList [ "I", "I", "I", "I" ] ]

        redrawn =
            emptyPlayfield
    in
    Array.fromList
        [ Array.fromList [ "`", "I", "I", "I", "I", "`" ]
        , Array.fromList [ "`", "`", "`", "`", "`", "`" ]
        , Array.fromList [ "`", "`", "`", "`", "`", "`" ]
        , Array.fromList [ "`", "`", "`", "`", "`", "`" ]
        , Array.fromList [ "`", "`", "`", "`", "`", "`" ]
        , Array.fromList [ "`", "`", "`", "`", "`", "`" ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick
