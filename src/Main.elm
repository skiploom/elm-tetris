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
    ( { playfield = addPieceToPlayfield (O oInitPosition) emptyPlayfield
      , activePiece = O oInitPosition
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | MoveLeft
    | MoveRight
    | SoftDrop
    | HardDrop
    | RotateClockwise
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

        RotateClockwise ->
            ( rotateClockwise model, Cmd.none )

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

        areSpacesEmpty position =
            List.all (isSpaceEmpty_ playfieldWithoutActivePiece) (positionToList position)
    in
    isWithinPlayfieldBounds destination && areSpacesEmpty destination


isWithinPlayfieldBounds : Position -> Bool
isWithinPlayfieldBounds position =
    List.all isWithinPlayfieldBoundsHelper (positionToList position)


isWithinPlayfieldBoundsHelper : ( Int, Int ) -> Bool
isWithinPlayfieldBoundsHelper ( x, y ) =
    x >= leftLimit && x <= rightLimit && y >= 0 && y <= downLimit


positionToList : Position -> List ( Int, Int )
positionToList pos =
    [ pos.point1, pos.point2, pos.point3, pos.point4 ]


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
    let
        moveFunction =
            case whichWay of
                Left ->
                    goLeft 1

                Right ->
                    goRight 1

                Down ->
                    goDown 1
    in
    setPosition (moveFunction (getPosition piece)) piece


goLeft : Int -> Position -> Position
goLeft numSpaces pos =
    mapPosition (Tuple.mapFirst ((+) -numSpaces)) pos


goRight : Int -> Position -> Position
goRight numSpaces pos =
    mapPosition (Tuple.mapFirst ((+) numSpaces)) pos


goDown : Int -> Position -> Position
goDown numSpaces pos =
    mapPosition (Tuple.mapSecond ((+) numSpaces)) pos


rotateClockwise : Model -> Model
rotateClockwise model =
    -- Try to rotate activePiece clockwise, depending on:
    --   1. Its current "rotation state"
    --   2. If it will collide
    --
    -- First pass:
    -- Don't worry about #2.
    -- Just get each piece to rotate.
    -- Let's hardcode the "deltas" between the I block in its
    -- horizontal state and its vertical state.
    -- So on "up key press", the I block should switch between
    -- horizontal and vertical.
    --
    -- Then we can care about the I block's proper "4 rotation states"
    -- next.
    --
    -- Then work out collisions.
    --
    -- Then work on the next pieces.
    --
    -- Don't worry about code cleanliness just yet.
    --
    -- Current state of I block:
    -- It always has position of {(n, y), (n+1, y), (n+2, y), (n+3, y)}
    -- i.e.
    -- {(1, 4), (2, 4), (3, 4), (4, 4)}
    --
    -- After rotation clockwise (first time from initial), it should be at:
    -- {(2, 3), (2,4), (2,5), (2,6)}
    -- (We are arbitrarily assuming that the (1,4) block, aka the second block
    -- is the center of rotation and so it will remain static. We can check the
    -- tetris guideline later to see the true behaviorm, but ignore for now.)
    --
    -- If we rotate again (second time from initial), then it should be at:
    -- {(3, 4), (2,4), (1,4), (0, 4)}
    --
    -- Rotating again-again (third time from initial) should give:
    -- {(2, 5), (2,4), (2,3), (2, 2)}
    --
    -- Fourth and final rotation should give:
    -- {(1, 4), (2,4), (3,4), (4, 4)}
    -- (This happens to be same state as when it started.)
    --
    -- So there are four rotation states:
    -- 1. 0 degrees
    -- 2. 90
    -- 3. 180
    -- 4. 270
    --
    -- And the deltas (specifically for I-block), between state-to-state:
    -- 0: {(0,0), (0,0), (0,0), (0,0)}
    -- 90: {(+1,-1), (+0,+0), (-1,+1), (-2,+2)}
    -- 180: {(-1,+1), (+0,+0), (+1,-1), (+2,-2)}
    -- 270: {(-1,-1), (+0,+0), (+1,+1), (+2,+2)}
    model


mapPosition : (( Int, Int ) -> ( Int, Int )) -> Position -> Position
mapPosition fn pos =
    { pos
        | point1 = fn pos.point1
        , point2 = fn pos.point2
        , point3 = fn pos.point3
        , point4 = fn pos.point4
    }


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
            goDown numMovableSpacesDown (getPosition model.activePiece)

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


isToppedOut : Model -> Bool
isToppedOut model =
    isPieceAtTop model && isPieceStuck model


isPieceAtTop : Model -> Bool
isPieceAtTop model =
    model.activePiece
        |> getPosition
        |> positionToList
        |> List.any (\point -> Tuple.second point == 0)


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


maybeLockPiece : Model -> ( Model, Cmd Msg )
maybeLockPiece model =
    if isToppedOut model then
        -- Start the game over.
        ( { playfield = addPieceToPlayfield (O oInitPosition) emptyPlayfield
          , activePiece = O oInitPosition
          }
        , Cmd.none
        )

    else if isPieceStuck model then
        ( model, newPiece )

    else
        ( model, Cmd.none )


lockPiece : Model -> Model
lockPiece model =
    { model | playfield = addPieceToPlayfield (O oInitPosition) (clearLines model.playfield), activePiece = O oInitPosition }


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

        "ArrowUp" ->
            RotateClockwise

        " " ->
            -- Space key
            HardDrop

        _ ->
            NoOp


randomPieceHelper : Random.Generator Piece
randomPieceHelper =
    Random.uniform (O oInitPosition)
        [ I iInitPosition
        , L lInitPosition
        , J jInitPosition
        , Z zInitPosition
        , S sInitPosition
        , T tInitPosition
        ]


oInitPosition : Position
oInitPosition =
    { point1 = ( 3, 0 )
    , point2 = ( 4, 0 )
    , point3 = ( 4, 1 )
    , point4 = ( 3, 1 )
    }


iInitPosition : Position
iInitPosition =
    { point1 = ( 3, 0 )
    , point2 = ( 4, 0 )
    , point3 = ( 5, 0 )
    , point4 = ( 6, 0 )
    }


lInitPosition : Position
lInitPosition =
    { point1 = ( 3, 1 )
    , point2 = ( 4, 1 )
    , point3 = ( 5, 1 )
    , point4 = ( 5, 2 )
    }


jInitPosition : Position
jInitPosition =
    { point1 = ( 3, 0 )
    , point2 = ( 3, 1 )
    , point3 = ( 4, 1 )
    , point4 = ( 5, 1 )
    }


zInitPosition : Position
zInitPosition =
    { point1 = ( 3, 0 )
    , point2 = ( 4, 0 )
    , point3 = ( 4, 1 )
    , point4 = ( 5, 1 )
    }


sInitPosition : Position
sInitPosition =
    { point1 = ( 3, 1 )
    , point2 = ( 4, 1 )
    , point3 = ( 4, 0 )
    , point4 = ( 5, 0 )
    }


tInitPosition : Position
tInitPosition =
    { point1 = ( 3, 1 )
    , point2 = ( 4, 1 )
    , point3 = ( 4, 2 )
    , point4 = ( 5, 1 )
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


addPieceToPlayfield : Piece -> Playfield -> Playfield
addPieceToPlayfield piece playfield =
    updatePieceOnPlayfield (pieceToString piece) piece playfield


removePieceFromPlayfield : Piece -> Playfield -> Playfield
removePieceFromPlayfield piece playfield =
    updatePieceOnPlayfield emptySpace piece playfield


updatePieceOnPlayfield : Space -> Piece -> Playfield -> Playfield
updatePieceOnPlayfield space piece playfield =
    let
        position =
            getPosition piece
    in
    playfield
        |> updateSpaceOnPlayfield space position.point1
        |> updateSpaceOnPlayfield space position.point2
        |> updateSpaceOnPlayfield space position.point3
        |> updateSpaceOnPlayfield space position.point4


updateSpaceOnPlayfield : Space -> ( Int, Int ) -> Playfield -> Playfield
updateSpaceOnPlayfield newSpace ( x, y ) playfield =
    Array.get y playfield
        |> Maybe.withDefault Array.empty
        |> Array.set x newSpace
        |> (\newLine -> Array.set y newLine playfield)


showPlayfield : Playfield -> Html Msg
showPlayfield playfield =
    div
        [ style "font-family" "monospace"
        , style "font-size" "2.5rem"
        , style "text-align" "center"
        , style "padding" "1rem"
        , style "line-height" "1.5rem"
        ]
        (Array.toList (Array.map showLine playfield))


showLine : Array Space -> Html Msg
showLine line =
    div [] (Array.toList (Array.map showSpace line))


showSpace : Space -> Html Msg
showSpace space =
    span [ style "color" (spaceToColor space) ] [ text (showShape space) ]


showShape : Space -> String
showShape space =
    if space == emptySpace then
        "□"

    else
        "▣"


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
            -- Gray
            "#757575"


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
