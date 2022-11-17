module Main exposing (..)

import Array exposing (Array)
import Browser
import Browser.Events
import Html exposing (Html, br, button, div, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Json.Decode
import List.Extra
import Random
import Time



{-
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
    , activePiece : Piece
    }


type alias Playfield =
    Array (Array Space)


type alias SecondsElapsed =
    Int


type Piece
    = O Position
    | I Position
    | Z Position
    | S Position
    | L Position
    | J Position
    | T Position


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
    | NewPiece Piece
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            maybeLockPiece (maybeRefreshPlayfield model Down)

        MoveLeft ->
            ( maybeRefreshPlayfield model Left, Cmd.none )

        MoveRight ->
            ( maybeRefreshPlayfield model Right, Cmd.none )

        SoftDrop ->
            ( maybeRefreshPlayfield model Down, Cmd.none )

        HardDrop ->
            ( hardDrop model, newPiece )

        NewPiece piece ->
            ( { model | playfield = addPieceToPlayfield piece (clearLines model.playfield), activePiece = piece }, Cmd.none )

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

        Z _ ->
            Z newPosition

        S _ ->
            S newPosition

        L _ ->
            L newPosition

        J _ ->
            J newPosition

        T _ ->
            T newPosition


getPosition : Piece -> Position
getPosition piece =
    case piece of
        O position ->
            position

        I position ->
            position

        Z position ->
            position

        S position ->
            position

        L position ->
            position

        J position ->
            position

        T position ->
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


maybeLockPiece : Model -> ( Model, Cmd Msg )
maybeLockPiece model =
    if isToppedOut model then
        -- Start the game over.
        ( { playfield = addPieceToPlayfield (O initialPosition) emptyPlayfield
          , activePiece = O initialPosition
          }
        , Cmd.none
        )

    else if isPieceStuck model then
        ( model, newPiece )

    else
        ( model, Cmd.none )


lockPiece : Model -> Model
lockPiece model =
    { model | playfield = addPieceToPlayfield (O initialPosition) (clearLines model.playfield), activePiece = O initialPosition }


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


randomPieceHelper : Random.Generator Piece
randomPieceHelper =
    Random.uniform (O initialPosition)
        [ I initialPosition
        , L initialPosition
        , J initialPosition
        , Z initialPosition
        , S initialPosition
        , T initialPosition
        ]


newPiece : Cmd Msg
newPiece =
    Random.generate NewPiece randomPieceHelper



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
    updatePieceOnPlayfield piece playfield (pieceToString piece)


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
    div
        [ style "font-family" "monospace"
        , style "font-size" "1.5rem"
        , style "text-align" "center"
        , style "padding" "1rem"
        ]
        (Array.toList (Array.map showLine playfield))


showLine : Array Space -> Html Msg
showLine line =
    div [] (Array.toList (Array.map showSpace line))


showSpace : Space -> Html Msg
showSpace space =
    span [ style "color" (spaceToColor space) ] [ text space ]


spaceToColor : Space -> String
spaceToColor space =
    case space of
        "O" ->
            -- Yellow
            "#FFF176"

        "I" ->
            -- Cyan
            "#4DD0E1"

        "Z" ->
            -- Pink
            "#F06292"

        "S" ->
            -- Green
            "#81C784"

        "L" ->
            -- Orange
            "#E65100"

        "J" ->
            -- Blue
            "#0D47A1"

        "T" ->
            -- Purple
            "#BA68C8"

        _ ->
            -- Light Gray
            "#F5F5F5"


pieceToString : Piece -> Space
pieceToString piece =
    case piece of
        O _ ->
            "O"

        I _ ->
            "I"

        Z _ ->
            "Z"

        S _ ->
            "S"

        L _ ->
            "L"

        J _ ->
            "J"

        T _ ->
            "T"


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
