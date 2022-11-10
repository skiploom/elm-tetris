module Main exposing (..)

import Array exposing (Array)
import Browser
import Html exposing (Html, br, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Time



-- MAIN


main =
    Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }



-- MODEL


type alias Model =
    { playfield : Playfield
    , secondsElapsed : SecondsElapsed
    , activePiece : Piece
    }


type alias Playfield =
    Array (Array String)


type alias SecondsElapsed =
    Int


type Piece
    = O Position


type alias Position =
    ( Int, Int )


type WhichWay
    = L
    | R
    | D


init : () -> ( Model, Cmd Msg )
init _ =
    ( { playfield = emptyPlayfield
      , secondsElapsed = 0
      , activePiece = O ( 1, 0 )
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | Left
    | Right
    | Down


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ activePiece, playfield, secondsElapsed } as model) =
    case msg of
        Down ->
            ( { model | activePiece = movePiece activePiece D }, Cmd.none )

        Left ->
            ( { model | activePiece = movePiece activePiece L }, Cmd.none )

        Right ->
            ( { model | activePiece = movePiece activePiece R }, Cmd.none )

        Tick time ->
            ( { model | secondsElapsed = secondsElapsed + 1 }, Cmd.none )


movePiece : Piece -> WhichWay -> Piece
movePiece piece whichWay =
    case whichWay of
        L ->
            O <| Tuple.mapFirst goLeft (getPosition piece)

        R ->
            O <| Tuple.mapFirst goRight (getPosition piece)

        D ->
            O <| Tuple.mapSecond goDown (getPosition piece)


goLeft : Int -> Int
goLeft curr =
    max leftLimit (curr - 1)


goRight : Int -> Int
goRight curr =
    min rightLimit (curr + 1)


goDown : Int -> Int
goDown curr =
    min downLimit (curr + 1)


getPosition : Piece -> Position
getPosition piece =
    case piece of
        O position ->
            position


leftLimit : Int
leftLimit =
    0


rightLimit : Int
rightLimit =
    9


downLimit : Int
downLimit =
    19



-- VIEW


view : Model -> Html Msg
view { playfield, activePiece } =
    div []
        [ showPlayfield playfield
        , button [ onClick Down ] [ text "Down." ]
        , button [ onClick Left ] [ text "<-" ]
        , button [ onClick Right ] [ text "->" ]
        , br [] []
        , br [] []
        , showPieceAndPlayfieldWIP playfield activePiece
        ]


emptyPlayfield : Playfield
emptyPlayfield =
    Array.repeat (downLimit + 1) (Array.repeat (rightLimit + 1) "`")


showPlayfield : Playfield -> Html Msg
showPlayfield playfield =
    div [ style "font-family" "monospace" ]
        (Array.toList (Array.map showRow playfield))


showRow : Array String -> Html Msg
showRow row =
    div [] [ text (String.join "" (Array.toList row)) ]


showPieceAndPlayfieldWIP : Playfield -> Piece -> Html Msg
showPieceAndPlayfieldWIP playfield piece =
    let
        playfield_ =
            emptyPlayfield

        (O ( x, y )) =
            piece

        playfieldWithClearedPiece =
            Array.get y playfield
                |> Maybe.withDefault Array.empty
                |> Array.set x "`"
                |> (\newRow -> Array.set y newRow playfield)

        newPlayfield =
            Array.get y playfieldWithClearedPiece
                |> Maybe.withDefault Array.empty
                |> Array.set x "o"
                |> (\newRow -> Array.set y newRow playfieldWithClearedPiece)
    in
    -- div [] [ text <| String.concat [ "(", String.fromInt x, ", ", String.fromInt y, ")" ] ]
    showPlayfield newPlayfield



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick
