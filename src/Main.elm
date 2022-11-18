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
    { point1 : ( Int, Int )
    , point2 : ( Int, Int )
    , point3 : ( Int, Int )
    , point4 : ( Int, Int )
    }


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
    setPositionTempHelper ( 1, 0 )



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
            maybeLockPiece (maybeRefreshPlayfield model Down)

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
    -- First, naively remove the active piece from the playfield
    -- Then, for each of the four spaces of the new location, check if those spaces are filled on the playfield
    -- If ANY are filled, return false. OW, return true
    let
        destination =
            getPosition <| movePiece model.activePiece whichWay

        playfieldWithoutActivePiece =
            removePieceFromPlayfield model.activePiece model.playfield

        isSpaceEmpty_ playfield_ ( x_, y_ ) =
            playfield_
                |> Array.get y_
                |> Maybe.withDefault Array.empty
                |> Array.get x_
                |> Maybe.withDefault emptySpace
                |> isSpaceEmpty
    in
    List.all (isSpaceEmpty_ playfieldWithoutActivePiece) [ destination.point1, destination.point2, destination.point3, destination.point4 ]


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
            setPosition (goLeftNew (getPosition piece)) piece

        Right ->
            setPosition (goRightNew (getPosition piece)) piece

        Down ->
            setPosition (goDownNew (getPosition piece)) piece


movePieceTempHelper : (( Int, Int ) -> ( Int, Int )) -> Position -> Position
movePieceTempHelper point1MoverFunction originalPosition =
    { point1 = point1MoverFunction originalPosition.point1
    , point2 = point1MoverFunction originalPosition.point1
    , point3 = point1MoverFunction originalPosition.point1
    , point4 = point1MoverFunction originalPosition.point1
    }


goLeftNew : Position -> Position
goLeftNew curr =
    { curr
        | point1 = Tuple.mapFirst goLeft curr.point1
        , point2 = Tuple.mapFirst goLeft curr.point2
        , point3 = Tuple.mapFirst goLeft curr.point3
        , point4 = Tuple.mapFirst goLeft curr.point4
    }


goRightNew : Position -> Position
goRightNew curr =
    { curr
        | point1 = Tuple.mapFirst goRight curr.point1
        , point2 = Tuple.mapFirst goRight curr.point2
        , point3 = Tuple.mapFirst goRight curr.point3
        , point4 = Tuple.mapFirst goRight curr.point4
    }


goDownNew : Position -> Position
goDownNew curr =
    { curr
        | point1 = Tuple.mapSecond goDown curr.point1
        , point2 = Tuple.mapSecond goDown curr.point2
        , point3 = Tuple.mapSecond goDown curr.point3
        , point4 = Tuple.mapSecond goDown curr.point4
    }


goDownNewNew : Int -> Position -> Position
goDownNewNew numSpaces curr =
    let
        foo y =
            min downLimit (y + numSpaces)
    in
    { curr
        | point1 = Tuple.mapSecond foo curr.point1
        , point2 = Tuple.mapSecond foo curr.point2
        , point3 = Tuple.mapSecond foo curr.point3
        , point4 = Tuple.mapSecond foo curr.point4
    }


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
        playfieldWithoutActivePiece =
            removePieceFromPlayfield model.activePiece model.playfield

        -- Try to find the highest space as early as possible by constantly going down until
        -- a filled space is hit.
        numMovableSpacesDown =
            hardDropHelper { model | playfield = playfieldWithoutActivePiece } True -1

        destination =
            goDownNewNew numMovableSpacesDown (getPosition model.activePiece)

        movedPiece =
            setPosition destination model.activePiece

        newPlayfield =
            addPieceToPlayfield movedPiece playfieldWithoutActivePiece
    in
    { model | activePiece = movedPiece, playfield = newPlayfield }


{-| Finds how many spaces the active piece can move down before colliding with existing pieces
-}
hardDropHelper : Model -> Bool -> Int -> Int
hardDropHelper model shouldContinue counter =
    case shouldContinue of
        True ->
            hardDropHelper { model | activePiece = movePiece model.activePiece Down } (not <| isPieceStuck model) (counter + 1)

        False ->
            counter


setPositionTempHelper : ( Int, Int ) -> Position
setPositionTempHelper point1 =
    { point1 = point1
    , point2 = point1
    , point3 = point1
    , point4 = point1
    }


isToppedOut : Model -> Bool
isToppedOut model =
    (Tuple.second (getPositionTempHelper <| getPosition model.activePiece) == 0) && isPieceStuck model


isPieceStuck : Model -> Bool
isPieceStuck model =
    isThereAPieceBelow model || isPieceAtBottom model


isThereAPieceBelow : Model -> Bool
isThereAPieceBelow model =
    not (canPieceMoveThatWay model Down)


isPieceAtBottom : Model -> Bool
isPieceAtBottom model =
    getPosition model.activePiece
        |> positionToList
        |> List.map Tuple.second
        |> List.any ((==) downLimit)


positionToList : Position -> List ( Int, Int )
positionToList position =
    [ position.point1
    , position.point2
    , position.point3
    , position.point4
    ]


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


isSpaceEmpty : Space -> Bool
isSpaceEmpty space =
    not <| isSpaceFull space


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
        [ I iPieceLocationTempHelper
        , L initialPosition
        , J initialPosition
        , Z initialPosition
        , S initialPosition
        , T initialPosition
        ]


iPieceLocationTempHelper : Position
iPieceLocationTempHelper =
    { point1 = ( 1, 0 )
    , point2 = ( 1, 1 )
    , point3 = ( 1, 2 )
    , point4 = ( 1, 3 )
    }


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


updatePieceOnPlayfield : Piece -> Playfield -> String -> Playfield
updatePieceOnPlayfield piece playfield str =
    let
        ( x, y ) =
            getPositionTempHelper <| getPosition piece
    in
    Array.get y playfield
        |> Maybe.withDefault Array.empty
        |> Array.set x str
        |> (\newLine -> Array.set y newLine playfield)


addPieceToPlayfield : Piece -> Playfield -> Playfield
addPieceToPlayfield piece playfield =
    -- Get the four elements from the playfield that correspond to the four parts of piece.location
    -- At those playfield elements, replace whatever is there with the piece's shape.
    let
        ( str, position ) =
            ( pieceToString piece, getPosition piece )

        redraw ( x_, y_ ) playfield_ =
            Array.get y_ playfield_
                |> Maybe.withDefault Array.empty
                |> Array.set x_ str
                |> (\newLine -> Array.set y_ newLine playfield_)
    in
    playfield
        |> redraw position.point1
        |> redraw position.point2
        |> redraw position.point3
        |> redraw position.point4


removePieceFromPlayfield : Piece -> Playfield -> Playfield
removePieceFromPlayfield piece playfield =
    -- Get the four elements from the playfield that correspond to the four parts of piece.location
    -- At those playfield elements, replace whatever is there with an empty space.
    let
        str =
            "`"

        position =
            getPosition piece

        redraw ( x_, y_ ) playfield_ =
            Array.get y_ playfield_
                |> Maybe.withDefault Array.empty
                |> Array.set x_ str
                |> (\newLine -> Array.set y_ newLine playfield_)
    in
    playfield
        |> redraw position.point1
        |> redraw position.point2
        |> redraw position.point3
        |> redraw position.point4


getPositionTempHelper : Position -> ( Int, Int )
getPositionTempHelper position =
    position.point1


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
