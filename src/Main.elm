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
   -- TODO Make simple clockwise rotation logic for all tetrominoes
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
    = O Position RotationState
    | I Position RotationState
    | Z Position RotationState
    | S Position RotationState
    | L Position RotationState
    | J Position RotationState
    | T Position RotationState


type RotationState
    = Rotated0
    | Rotated90
    | Rotated180
    | Rotated270


type alias RotationDelta =
    { d1 : ( Int, Int )
    , d2 : ( Int, Int )
    , d3 : ( Int, Int )
    , d4 : ( Int, Int )
    }


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
    ( { playfield = addPieceToPlayfield (O oInitPosition Rotated0) emptyPlayfield
      , activePiece = O oInitPosition Rotated0
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
    in
    isWithinPlayfieldBounds destination && areSpacesEmpty destination model


canPieceRotateThatWay : Model -> Bool
canPieceRotateThatWay model =
    let
        destination =
            getPosition <| rotateClockwiseHelper model.activePiece
    in
    isWithinPlayfieldBounds destination && areSpacesEmpty destination model


areSpacesEmpty : Position -> Model -> Bool
areSpacesEmpty destination model =
    let
        playfieldWithoutActivePiece =
            removePieceFromPlayfield model.activePiece model.playfield

        isSpaceEmpty_ playfield_ ( x_, y_ ) =
            isSpaceEmpty <| getSpaceAt ( x_, y_ ) playfield_
    in
    List.all (isSpaceEmpty_ playfieldWithoutActivePiece) (positionToList destination)


getSpaceAt : ( Int, Int ) -> Playfield -> Space
getSpaceAt ( x, y ) playfield =
    playfield
        |> Array.get y
        |> Maybe.withDefault Array.empty
        |> Array.get x
        |> Maybe.withDefault emptySpace


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
        movedPiece =
            movePiece model.activePiece whichWay

        newPlayfield =
            refreshPlayfieldHelper model.activePiece movedPiece model.playfield
    in
    { model | activePiece = movedPiece, playfield = newPlayfield }


refreshPlayfieldHelper : Piece -> Piece -> Playfield -> Playfield
refreshPlayfieldHelper oldPiece newPiece_ playfield =
    playfield
        |> removePieceFromPlayfield oldPiece
        |> addPieceToPlayfield newPiece_


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
    if canPieceRotateThatWay model then
        { model | activePiece = rotateClockwiseHelper model.activePiece, playfield = refreshPlayfieldHelper model.activePiece (rotateClockwiseHelper model.activePiece) model.playfield }

    else
        model


rotateClockwiseHelper : Piece -> Piece
rotateClockwiseHelper piece =
    piece
        |> changeRotationState
        |> rotatePosition


getRotationState : Piece -> RotationState
getRotationState piece =
    case piece of
        I _ rotationState ->
            rotationState

        Z _ rotationState ->
            rotationState

        T _ rotationState ->
            rotationState

        J _ rotationState ->
            rotationState

        L _ rotationState ->
            rotationState

        O _ rotationState ->
            rotationState

        S _ _ ->
            Rotated0


{-| Assume clockwise.
-}
changeRotationState : Piece -> Piece
changeRotationState piece =
    case piece of
        I position rotationState ->
            I position (cycleRotationState rotationState)

        Z position rotationState ->
            Z position (cycleRotationState rotationState)

        T position rotationState ->
            T position (cycleRotationState rotationState)

        J position rotationState ->
            J position (cycleRotationState rotationState)

        L position rotationState ->
            L position (cycleRotationState rotationState)

        O position rotationState ->
            O position (cycleRotationState rotationState)

        S position rotationState ->
            S position (cycleRotationState rotationState)


{-| Assume clockwise.
-}
cycleRotationState : RotationState -> RotationState
cycleRotationState currentState =
    case currentState of
        Rotated0 ->
            Rotated90

        Rotated90 ->
            Rotated180

        Rotated180 ->
            Rotated270

        Rotated270 ->
            Rotated0


rotatePosition : Piece -> Piece
rotatePosition piece =
    applyRotationDelta (getRotationDelta piece) piece


{-| Eventually this should handle counterclockwise. Assume clockwise for now.
-}
getRotationDelta : Piece -> RotationDelta
getRotationDelta piece =
    case piece of
        I _ Rotated90 ->
            buildRotationDelta ( 2, -1 ) ( 1, 0 ) ( 0, 1 ) ( -1, 2 )

        I _ Rotated180 ->
            buildRotationDelta ( -2, 2 ) ( -1, 1 ) ( 0, 0 ) ( 1, -1 )

        I _ Rotated270 ->
            buildRotationDelta ( 1, -2 ) ( 0, -1 ) ( -1, 0 ) ( -2, 1 )

        I _ Rotated0 ->
            buildRotationDelta ( -1, 1 ) ( 0, 0 ) ( 1, -1 ) ( 2, -2 )

        Z _ Rotated90 ->
            buildRotationDelta ( 2, 0 ) ( 1, 1 ) ( 0, 0 ) ( -1, 1 )

        Z _ Rotated180 ->
            buildRotationDelta ( 0, 2 ) ( -1, 1 ) ( 0, 0 ) ( -1, -1 )

        Z _ Rotated270 ->
            buildRotationDelta ( -2, 0 ) ( -1, -1 ) ( 0, 0 ) ( 1, -1 )

        Z _ Rotated0 ->
            buildRotationDelta ( 0, -2 ) ( 1, -1 ) ( 0, 0 ) ( 1, 1 )

        T _ Rotated90 ->
            buildRotationDelta ( 1, -1 ) ( 0, 0 ) ( 1, 1 ) ( -1, 1 )

        T _ Rotated180 ->
            buildRotationDelta ( 1, 1 ) ( 0, 0 ) ( -1, 1 ) ( -1, -1 )

        T _ Rotated270 ->
            buildRotationDelta ( -1, 1 ) ( 0, 0 ) ( -1, -1 ) ( 1, -1 )

        T _ Rotated0 ->
            buildRotationDelta ( -1, -1 ) ( 0, 0 ) ( 1, -1 ) ( 1, 1 )

        J _ Rotated90 ->
            buildRotationDelta ( 2, 0 ) ( 1, -1 ) ( 0, 0 ) ( -1, 1 )

        J _ Rotated180 ->
            buildRotationDelta ( 0, 2 ) ( 1, 1 ) ( 0, 0 ) ( -1, -1 )

        J _ Rotated270 ->
            buildRotationDelta ( -2, 0 ) ( -1, 1 ) ( 0, 0 ) ( 1, -1 )

        J _ Rotated0 ->
            buildRotationDelta ( 0, -2 ) ( -1, -1 ) ( 0, 0 ) ( 1, 1 )

        L _ Rotated90 ->
            buildRotationDelta ( 1, -1 ) ( 0, 0 ) ( -1, 1 ) ( 0, 2 )

        L _ Rotated180 ->
            buildRotationDelta ( 1, 1 ) ( 0, 0 ) ( -1, -1 ) ( -2, 0 )

        L _ Rotated270 ->
            buildRotationDelta ( -1, 1 ) ( 0, 0 ) ( 1, -1 ) ( 0, -2 )

        L _ Rotated0 ->
            buildRotationDelta ( -1, -1 ) ( 0, 0 ) ( 1, 1 ) ( 2, 0 )

        O _ _ ->
            buildRotationDelta ( 0, 0 ) ( 0, 0 ) ( 0, 0 ) ( 0, 0 )

        S _ Rotated90 ->
            buildRotationDelta ( 1, -1 ) ( 0, 0 ) ( 1, 1 ) ( 0, 2 )

        S _ Rotated180 ->
            buildRotationDelta ( 1, 1 ) ( 0, 0 ) ( -1, 1 ) ( -2, 0 )

        S _ Rotated270 ->
            buildRotationDelta ( -1, 1 ) ( 0, 0 ) ( -1, -1 ) ( 0, -2 )

        S _ Rotated0 ->
            buildRotationDelta ( -1, -1 ) ( 0, 0 ) ( 1, -1 ) ( 2, 0 )


buildRotationDelta : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int ) -> ( Int, Int ) -> RotationDelta
buildRotationDelta d1 d2 d3 d4 =
    { d1 = d1
    , d2 = d2
    , d3 = d3
    , d4 = d4
    }


applyRotationDelta : RotationDelta -> Piece -> Piece
applyRotationDelta { d1, d2, d3, d4 } piece =
    let
        pos =
            getPosition piece

        ( x1, y1 ) =
            d1

        ( x2, y2 ) =
            d2

        ( x3, y3 ) =
            d3

        ( x4, y4 ) =
            d4
    in
    setPosition
        { pos
            | point1 = Tuple.mapBoth ((+) x1) ((+) y1) pos.point1
            , point2 = Tuple.mapBoth ((+) x2) ((+) y2) pos.point2
            , point3 = Tuple.mapBoth ((+) x3) ((+) y3) pos.point3
            , point4 = Tuple.mapBoth ((+) x4) ((+) y4) pos.point4
        }
        piece


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
        O _ rotationState ->
            O newPosition rotationState

        I _ rotationState ->
            I newPosition rotationState

        Z _ rotationState ->
            Z newPosition rotationState

        S _ rotationState ->
            S newPosition rotationState

        L _ rotationState ->
            L newPosition rotationState

        J _ rotationState ->
            J newPosition rotationState

        T _ rotationState ->
            T newPosition rotationState


getPosition : Piece -> Position
getPosition piece =
    case piece of
        O position _ ->
            position

        I position _ ->
            position

        Z position _ ->
            position

        S position _ ->
            position

        L position _ ->
            position

        J position _ ->
            position

        T position _ ->
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
        ( { playfield = addPieceToPlayfield (O oInitPosition Rotated0) emptyPlayfield
          , activePiece = O oInitPosition Rotated0
          }
        , Cmd.none
        )

    else if isPieceStuck model then
        ( model, newPiece )

    else
        ( model, Cmd.none )


lockPiece : Model -> Model
lockPiece model =
    { model | playfield = addPieceToPlayfield (O oInitPosition Rotated0) (clearLines model.playfield), activePiece = O oInitPosition Rotated0 }


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
    Random.uniform (O oInitPosition Rotated0)
        [ I iInitPosition Rotated0
        , L lInitPosition Rotated0
        , J jInitPosition Rotated0
        , Z zInitPosition Rotated0
        , S sInitPosition Rotated0
        , T tInitPosition Rotated0
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
    , point4 = ( 5, 0 )
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
    { point1 = ( 3, 2 )
    , point2 = ( 4, 2 )
    , point3 = ( 4, 1 )
    , point4 = ( 5, 2 )
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
        O _ _ ->
            "O"

        I _ _ ->
            "I"

        Z _ _ ->
            "Z"

        S _ _ ->
            "S"

        L _ _ ->
            "L"

        J _ _ ->
            "J"

        T _ _ ->
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
