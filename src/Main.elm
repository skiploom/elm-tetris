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
    ( { playfield = addPieceToPlayfield (O initialPosition) emptyPlayfield
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
            ( maybeRefreshPlayfield model D, Cmd.none )

        Left ->
            ( maybeRefreshPlayfield model L, Cmd.none )

        Right ->
            ( maybeRefreshPlayfield model R, Cmd.none )

        Tick time ->
            ( maybeLockPiece (maybeRefreshPlayfield model D), Cmd.none )


maybeRefreshPlayfield : Model -> WhichWay -> Model
maybeRefreshPlayfield model whichWay =
    if canPieceMoveThatWay model whichWay then
        refreshPlayfield model whichWay

    else
        model


canPieceMoveThatWay : Model -> WhichWay -> Bool
canPieceMoveThatWay model whichWay =
    let
        (O ( x, y )) =
            movePiece model.activePiece whichWay

        spaceBelow =
            model.playfield
                |> Array.get y
                |> Maybe.withDefault Array.empty
                |> Array.get x
                |> Maybe.withDefault "`"
    in
    spaceBelow == "`"


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


isToppedOut : Model -> Bool
isToppedOut model =
    (Tuple.second (getPosition model.activePiece) == 0) && isPieceStuck model


isPieceStuck : Model -> Bool
isPieceStuck model =
    isThereAPieceBelow model || isPieceAtBottom model


isThereAPieceBelow : Model -> Bool
isThereAPieceBelow model =
    let
        (O ( x, y )) =
            model.activePiece

        spaceBelow =
            model.playfield
                |> Array.get (y + 1)
                |> Maybe.withDefault Array.empty
                |> Array.get x
                |> Maybe.withDefault "`"
    in
    spaceBelow /= "`"


isPieceAtBottom : Model -> Bool
isPieceAtBottom model =
    Tuple.second (getPosition model.activePiece) == downLimit


maybeLockPiece : Model -> Model
maybeLockPiece model =
    if isToppedOut model then
        -- Start over.
        { playfield = addPieceToPlayfield (O initialPosition) emptyPlayfield
        , secondsElapsed = 0
        , activePiece = O initialPosition
        }

    else if isPieceStuck model then
        { model | playfield = addPieceToPlayfield (O initialPosition) model.playfield, activePiece = O initialPosition, secondsElapsed = 0 }

    else
        { model | secondsElapsed = model.secondsElapsed + 1 }


isThereALineClear : Model -> Bool
isThereALineClear _ =
    -- TODO Determine if the playfield has any filled lines
    False


clearLines : List Int -> Playfield -> Playfield
clearLines rows playfield =
    -- TODO Clear lines at given rows
    playfield



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
