module Main exposing (..)

import Array exposing (Array)
import Browser
import Browser.Events
import Html exposing (Html, br, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Json.Decode
import List.Extra
import Time



{-
   -- TODO Make at least one new piece
   -- TODO Randomize which piece is generated
   -- TODO Make pieces not just one space large
   -- TODO Make simple clockwise rotation logic
-}
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
    Array (Array Space)


type alias SecondsElapsed =
    Int


type Piece
    = O Position
    | I Position


type alias Position =
    ( Int, Int )


type WhichWay
    = Left
    | Right
    | Down


type alias Space =
    String


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
    | MoveLeft
    | MoveRight
    | SoftDrop
    | HardDrop
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ activePiece, playfield, secondsElapsed } as model) =
    case msg of
        Tick time ->
            ( maybeLockPiece (maybeRefreshPlayfield model Down), Cmd.none )

        MoveLeft ->
            ( maybeRefreshPlayfield model Left, Cmd.none )

        MoveRight ->
            ( maybeRefreshPlayfield model Right, Cmd.none )

        SoftDrop ->
            ( maybeRefreshPlayfield model Down, Cmd.none )

        HardDrop ->
            ( hardDrop model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


maybeRefreshPlayfield : Model -> WhichWay -> Model
maybeRefreshPlayfield model whichWay =
    if canPieceMoveThatWay model whichWay then
        refreshPlayfield model whichWay

    else
        model


canPieceMoveThatWay : Model -> WhichWay -> Bool
canPieceMoveThatWay model whichWay =
    let
        ( x, y ) =
            getPosition <| movePiece model.activePiece whichWay

        spaceBelow =
            model.playfield
                |> Array.get y
                |> Maybe.withDefault Array.empty
                |> Array.get x
                |> Maybe.withDefault emptySpace
    in
    spaceBelow == emptySpace


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
        Left ->
            setPosition (Tuple.mapFirst goLeft (getPosition piece)) piece

        Right ->
            setPosition (Tuple.mapFirst goRight (getPosition piece)) piece

        Down ->
            setPosition (Tuple.mapSecond goDown (getPosition piece)) piece


goLeft : Int -> Int
goLeft curr =
    max leftLimit (curr - 1)


goRight : Int -> Int
goRight curr =
    min rightLimit (curr + 1)


goDown : Int -> Int
goDown curr =
    min downLimit (curr + 1)


setPosition : Position -> Piece -> Piece
setPosition newPosition piece =
    case piece of
        O _ ->
            O newPosition

        I _ ->
            I newPosition


getPosition : Piece -> Position
getPosition piece =
    case piece of
        O position ->
            position

        I position ->
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


hardDrop : Model -> Model
hardDrop model =
    let
        ( x, y ) =
            getPosition model.activePiece

        highestFilledY =
            findIndexForHardDrop <|
                findFilledSpaceIndicesInColumn <|
                    extractColumn x model.playfield

        movedPiece =
            setPosition ( x, max 0 (highestFilledY - 1) ) model.activePiece

        newPlayfield =
            model.playfield
                |> removePieceFromPlayfield model.activePiece
                |> addPieceToPlayfield movedPiece
    in
    lockPiece { model | activePiece = movedPiece, playfield = newPlayfield }


findIndexForHardDrop : List Int -> Int
findIndexForHardDrop filledIndices =
    case filledIndices of
        [] ->
            downLimit + 1

        [ _ ] ->
            downLimit + 1

        atLeastTwo ->
            List.Extra.getAt 1 atLeastTwo
                |> Maybe.withDefault (downLimit + 1)


{-| Includes the space filled by the currently active piece for simplicity
-}
findFilledSpaceIndicesInColumn : Array Space -> List Int
findFilledSpaceIndicesInColumn extractedColumn =
    List.Extra.findIndices isSpaceFull (Array.toList extractedColumn)


extractColumn : Int -> Playfield -> Array Space
extractColumn columnNumber playfield =
    Array.map (Array.get columnNumber >> Maybe.withDefault emptySpace) playfield


isToppedOut : Model -> Bool
isToppedOut model =
    (Tuple.second (getPosition model.activePiece) == 0) && isPieceStuck model


isPieceStuck : Model -> Bool
isPieceStuck model =
    isThereAPieceBelow model || isPieceAtBottom model


isThereAPieceBelow : Model -> Bool
isThereAPieceBelow model =
    not (canPieceMoveThatWay model Down)


isPieceAtBottom : Model -> Bool
isPieceAtBottom model =
    Tuple.second (getPosition model.activePiece) == downLimit


maybeLockPiece : Model -> Model
maybeLockPiece model =
    if isToppedOut model then
        -- Start the game over.
        { playfield = addPieceToPlayfield (O initialPosition) emptyPlayfield
        , secondsElapsed = 0
        , activePiece = O initialPosition
        }

    else if isPieceStuck model then
        lockPiece model

    else
        { model | secondsElapsed = model.secondsElapsed + 1 }


lockPiece : Model -> Model
lockPiece model =
    { model | playfield = addPieceToPlayfield (O initialPosition) (clearLines model.playfield), activePiece = O initialPosition, secondsElapsed = 0 }


isLineFull : Array Space -> Bool
isLineFull line =
    List.all isSpaceFull (Array.toList line)


isSpaceFull : Space -> Bool
isSpaceFull space =
    space /= emptySpace


clearLines : Playfield -> Playfield
clearLines playfield =
    let
        playfieldSubsetWithClearedLines =
            Array.filter (\line -> not (isLineFull line)) playfield

        numClearedLines =
            Array.length emptyPlayfield - Array.length playfieldSubsetWithClearedLines

        refreshedPlayfield =
            Array.append (Array.repeat numClearedLines emptyLine) playfieldSubsetWithClearedLines
    in
    refreshedPlayfield


keyDecoder : Json.Decode.Decoder Msg
keyDecoder =
    Json.Decode.map keyToAction (Json.Decode.field "key" Json.Decode.string)


keyToAction : String -> Msg
keyToAction string =
    case string of
        "ArrowLeft" ->
            MoveLeft

        "ArrowRight" ->
            MoveRight

        "ArrowDown" ->
            SoftDrop

        " " ->
            -- Space key
            HardDrop

        _ ->
            NoOp



-- VIEW


view : Model -> Html Msg
view { playfield, activePiece } =
    div []
        [ showPlayfield playfield
        ]


emptyPlayfield : Playfield
emptyPlayfield =
    Array.repeat (downLimit + 1) emptyLine


emptyLine : Array Space
emptyLine =
    Array.repeat (rightLimit + 1) emptySpace


removePieceFromPlayfield : Piece -> Playfield -> Playfield
removePieceFromPlayfield piece playfield =
    updatePieceOnPlayfield piece playfield emptySpace


addPieceToPlayfield : Piece -> Playfield -> Playfield
addPieceToPlayfield piece playfield =
    updatePieceOnPlayfield piece playfield (showPiece piece)


updatePieceOnPlayfield : Piece -> Playfield -> String -> Playfield
updatePieceOnPlayfield piece playfield str =
    let
        ( x, y ) =
            getPosition piece
    in
    Array.get y playfield
        |> Maybe.withDefault Array.empty
        |> Array.set x str
        |> (\newLine -> Array.set y newLine playfield)


showPlayfield : Playfield -> Html Msg
showPlayfield playfield =
    div [ style "font-family" "monospace" ]
        (Array.toList (Array.map showLine playfield))


showLine : Array Space -> Html Msg
showLine line =
    div [] [ text (String.join "" (Array.toList line)) ]


showPiece : Piece -> Space
showPiece piece =
    case piece of
        O _ ->
            "o"

        I _ ->
            "I"


emptySpace : Space
emptySpace =
    "`"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 1000 Tick
        , Browser.Events.onKeyDown keyDecoder
        ]
