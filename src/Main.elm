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
    ( { playfield = initPlayfield
      , secondsElapsed = 0
      , activePiece = O initialPosition
      }
    , Cmd.none
    )


initialPosition : Position
initialPosition =
    ( 1, 0 )



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
            ( refreshPlayfield model D, Cmd.none )

        Left ->
            ( refreshPlayfield model L, Cmd.none )

        Right ->
            ( refreshPlayfield model R, Cmd.none )

        Tick time ->
            ( maybeLockPiece model, Cmd.none )


refreshPlayfield : Model -> WhichWay -> Model
refreshPlayfield model whichWay =
    let
        oldPiece =
            model.activePiece

        movedPiece =
            movePiece model.activePiece whichWay

        newPlayfield =
            model.playfield
                |> removePieceFromPlayfield oldPiece
                |> addPieceToPlayfield movedPiece
    in
    { model | activePiece = movedPiece, playfield = newPlayfield }


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


isPieceAtBottom : Model -> Bool
isPieceAtBottom model =
    Tuple.second (getPosition model.activePiece) == downLimit


maybeLockPiece : Model -> Model
maybeLockPiece model =
    if isPieceAtBottom model then
        { model | playfield = initPlayfield, activePiece = O initialPosition, secondsElapsed = 0 }

    else
        { model | secondsElapsed = model.secondsElapsed + 1 }



-- VIEW


view : Model -> Html Msg
view { playfield, activePiece } =
    div []
        [ button [ onClick Down ] [ text "Down." ]
        , button [ onClick Left ] [ text "<-" ]
        , button [ onClick Right ] [ text "->" ]
        , br [] []
        , br [] []
        , showPlayfield playfield
        ]


initPlayfield : Playfield
initPlayfield =
    addPieceToPlayfield (O initialPosition) emptyPlayfield


emptyPlayfield : Playfield
emptyPlayfield =
    Array.repeat (downLimit + 1) (Array.repeat (rightLimit + 1) "`")


removePieceFromPlayfield : Piece -> Playfield -> Playfield
removePieceFromPlayfield piece playfield =
    updatePieceOnPlayfield piece playfield "`"


addPieceToPlayfield : Piece -> Playfield -> Playfield
addPieceToPlayfield piece playfield =
    updatePieceOnPlayfield piece playfield "o"


updatePieceOnPlayfield : Piece -> Playfield -> String -> Playfield
updatePieceOnPlayfield (O ( x, y )) playfield str =
    Array.get y playfield
        |> Maybe.withDefault Array.empty
        |> Array.set x str
        |> (\newRow -> Array.set y newRow playfield)


showPlayfield : Playfield -> Html Msg
showPlayfield playfield =
    div [ style "font-family" "monospace" ]
        (Array.toList (Array.map showRow playfield))


showRow : Array String -> Html Msg
showRow row =
    div [] [ text (String.join "" (Array.toList row)) ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick
