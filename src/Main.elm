module Main exposing (..)

import Array exposing (Array)
import Browser
import Browser.Events
import Html exposing (Html, br, button, div, li, span, strong, text, ul)
import Html.Attributes exposing (class, id)
import Html.Events exposing (onClick)
import Json.Decode
import List.Extra
import List.Nonempty exposing (Nonempty(..))
import Random
import Random.List
import Svg
import Svg.Attributes exposing (fill, height, rx, ry, stroke, strokeWidth, viewBox, width, x, y)
import Time



-- MAIN


main =
    Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }



-- MODEL


type alias Model =
    { activePiece : Piece
    , nextPieceQueue : Nonempty Piece
    , heldPiece : Maybe Piece
    , numPiecesGenerated : Int
    , hasAlreadySwapped : Bool
    , playfield : Playfield
    , windowSize : WindowSize
    }


type Piece
    = Piece Shape Position RotationState


type Shape
    = I
    | O
    | T
    | S
    | Z
    | J
    | L


type alias Position =
    { point1 : ( Int, Int )
    , point2 : ( Int, Int )
    , point3 : ( Int, Int )
    , point4 : ( Int, Int )
    }


type RotationState
    = Rotated0
    | Rotated90
    | Rotated180
    | Rotated270


type RotationDirection
    = Clockwise
    | CounterClockwise
    | Flip180


type MoveDirection
    = Left
    | Right
    | Down


type alias Playfield =
    Array (Array Space)


type Space
    = Filled Shape
    | Empty


type alias Window =
    { width : Int
    , height : Int
    }


type WindowSize
    = Mobile
    | Small
    | Medium
    | Large
    | ExtraLarge
    | ExtraExtraLarge


init : Json.Decode.Value -> ( Model, Cmd Msg )
init flags =
    newGame (decodeWindowFlags flags)



-- UPDATE


type Msg
    = NewGame (Nonempty Piece)
    | MoveLeft
    | MoveRight
    | SoftDrop
    | HardDrop
    | Rotate RotationDirection
    | Swap
    | GenerateBag (Nonempty Piece)
    | Tick Time.Posix
    | GotResizedWindow WindowSize
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame pieces ->
            ( { model
                | playfield = addPieceToPlayfield (List.Nonempty.head pieces) (clearLines model.playfield)
                , activePiece = List.Nonempty.head pieces
                , nextPieceQueue = List.Nonempty.pop pieces
              }
            , Cmd.none
            )

        MoveLeft ->
            ( maybeRefreshPlayfield model Left, Cmd.none )

        MoveRight ->
            ( maybeRefreshPlayfield model Right, Cmd.none )

        SoftDrop ->
            maybeLockPiece (maybeRefreshPlayfield model Down)

        HardDrop ->
            generateNextPiece (hardDrop model)

        Rotate direction ->
            ( rotate direction model, Cmd.none )

        Swap ->
            maybeSwap model

        GenerateBag pieces ->
            ( { model | nextPieceQueue = List.Nonempty.append model.nextPieceQueue pieces }, Cmd.none )

        Tick time ->
            maybeLockPiece (maybeRefreshPlayfield model Down)

        GotResizedWindow size ->
            ( { model | windowSize = size }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


maybeRefreshPlayfield : Model -> MoveDirection -> Model
maybeRefreshPlayfield model moveDirection =
    if canPieceMoveThatWay model moveDirection then
        refreshPlayfield model moveDirection

    else
        model


canPieceMoveThatWay : Model -> MoveDirection -> Bool
canPieceMoveThatWay model moveDirection =
    let
        destination =
            getPosition (movePiece model.activePiece moveDirection)
    in
    isWithinPlayfieldBounds destination && areSpacesEmpty destination model


canPieceRotateThatWay : RotationDirection -> Model -> Bool
canPieceRotateThatWay direction model =
    let
        destination =
            getPosition (rotateHelper direction model.activePiece)
    in
    isWithinPlayfieldBounds destination && areSpacesEmpty destination model


areSpacesEmpty : Position -> Model -> Bool
areSpacesEmpty destination model =
    let
        playfieldWithoutActivePiece =
            removePieceFromPlayfield model.activePiece model.playfield

        isSpaceEmpty_ playfield_ ( x_, y_ ) =
            isSpaceEmpty (getSpaceAt ( x_, y_ ) playfield_)
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


refreshPlayfield : Model -> MoveDirection -> Model
refreshPlayfield model moveDirection =
    let
        movedPiece =
            movePiece model.activePiece moveDirection

        newPlayfield =
            refreshPlayfieldHelper model.activePiece movedPiece model.playfield
    in
    { model | activePiece = movedPiece, playfield = newPlayfield }


refreshPlayfieldHelper : Piece -> Piece -> Playfield -> Playfield
refreshPlayfieldHelper oldPiece newPiece_ playfield =
    playfield
        |> removePieceFromPlayfield oldPiece
        |> addPieceToPlayfield newPiece_


movePiece : Piece -> MoveDirection -> Piece
movePiece piece moveDirection =
    let
        moveFunction =
            case moveDirection of
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


rotate : RotationDirection -> Model -> Model
rotate direction model =
    if canPieceRotateThatWay direction model then
        { model
            | activePiece = rotateHelper direction model.activePiece
            , playfield = refreshPlayfieldHelper model.activePiece (rotateHelper direction model.activePiece) model.playfield
        }

    else
        model


rotateHelper : RotationDirection -> Piece -> Piece
rotateHelper direction piece =
    let
        (Piece _ _ currentRotationState) =
            piece

        rotatedPiece =
            setRotationState direction piece

        (Piece _ _ newRotationState) =
            rotatedPiece
    in
    rotatePosition currentRotationState newRotationState rotatedPiece


setRotationState : RotationDirection -> Piece -> Piece
setRotationState direction (Piece shape position rotationState) =
    Piece shape position (cycleRotationState direction rotationState)


cycleRotationState : RotationDirection -> RotationState -> RotationState
cycleRotationState direction rotationState =
    case ( direction, rotationState ) of
        ( Clockwise, Rotated0 ) ->
            Rotated90

        ( Clockwise, Rotated90 ) ->
            Rotated180

        ( Clockwise, Rotated180 ) ->
            Rotated270

        ( Clockwise, Rotated270 ) ->
            Rotated0

        ( CounterClockwise, Rotated0 ) ->
            Rotated270

        ( CounterClockwise, Rotated90 ) ->
            Rotated0

        ( CounterClockwise, Rotated180 ) ->
            Rotated90

        ( CounterClockwise, Rotated270 ) ->
            Rotated180

        ( Flip180, Rotated0 ) ->
            Rotated180

        ( Flip180, Rotated90 ) ->
            Rotated270

        ( Flip180, Rotated180 ) ->
            Rotated0

        ( Flip180, Rotated270 ) ->
            Rotated90


rotatePosition : RotationState -> RotationState -> Piece -> Piece
rotatePosition currentState desiredState piece =
    let
        (Piece shape _ _) =
            piece

        rotationDelta =
            getPositionDiff (initialPositions shape currentState) (initialPositions shape desiredState)
    in
    applyRotationDelta rotationDelta piece


getPositionDiff : Position -> Position -> RotationDelta
getPositionDiff pos1 pos2 =
    let
        ( x1, y1 ) =
            pos2.point1

        ( x2, y2 ) =
            pos2.point2

        ( x3, y3 ) =
            pos2.point3

        ( x4, y4 ) =
            pos2.point4
    in
    { d1 = Tuple.mapBoth ((-) x1) ((-) y1) pos1.point1
    , d2 = Tuple.mapBoth ((-) x2) ((-) y2) pos1.point2
    , d3 = Tuple.mapBoth ((-) x3) ((-) y3) pos1.point3
    , d4 = Tuple.mapBoth ((-) x4) ((-) y4) pos1.point4
    }


type alias RotationDelta =
    { d1 : ( Int, Int )
    , d2 : ( Int, Int )
    , d3 : ( Int, Int )
    , d4 : ( Int, Int )
    }


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


mapRotationDelta : (( Int, Int ) -> ( Int, Int )) -> RotationDelta -> RotationDelta
mapRotationDelta fn rd =
    { rd
        | d1 = fn rd.d1
        , d2 = fn rd.d2
        , d3 = fn rd.d3
        , d4 = fn rd.d4
    }


mapPosition : (( Int, Int ) -> ( Int, Int )) -> Position -> Position
mapPosition fn pos =
    { pos
        | point1 = fn pos.point1
        , point2 = fn pos.point2
        , point3 = fn pos.point3
        , point4 = fn pos.point4
    }


setPosition : Position -> Piece -> Piece
setPosition newPosition (Piece shape _ rotationState) =
    Piece shape newPosition rotationState


getPosition : Piece -> Position
getPosition (Piece _ position _) =
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
    { model | activePiece = movedPiece, playfield = newPlayfield, hasAlreadySwapped = False, numPiecesGenerated = model.numPiecesGenerated + 1 }


{-| Finds how many spaces the active piece can move down before colliding with existing pieces
-}
hardDropHelper : Model -> Bool -> Int -> Int
hardDropHelper model shouldContinue counter =
    case shouldContinue of
        True ->
            hardDropHelper { model | activePiece = movePiece model.activePiece Down } (not (isPieceStuck model)) (counter + 1)

        False ->
            counter


maybeSwap : Model -> ( Model, Cmd Msg )
maybeSwap model =
    if model.hasAlreadySwapped then
        ( model, Cmd.none )

    else
        swap model


swap : Model -> ( Model, Cmd Msg )
swap model =
    case model.heldPiece of
        Nothing ->
            generateNextPiece
                { model
                    | heldPiece = Just (buildPiece (getShape model.activePiece))
                    , hasAlreadySwapped = True
                    , numPiecesGenerated = model.numPiecesGenerated + 1
                    , playfield = removePieceFromPlayfield model.activePiece model.playfield
                }

        Just heldPiece ->
            ( { model
                | heldPiece = Just (buildPiece (getShape model.activePiece))
                , activePiece = heldPiece
                , hasAlreadySwapped = True
                , playfield = refreshPlayfieldHelper model.activePiece heldPiece model.playfield
              }
            , Cmd.none
            )


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
        newGame model.windowSize

    else if isPieceStuck model then
        generateNextPiece
            { model
                | hasAlreadySwapped = False
                , numPiecesGenerated = model.numPiecesGenerated + 1
            }

    else
        ( model, Cmd.none )


newGame : WindowSize -> ( Model, Cmd Msg )
newGame windowSize =
    ( { activePiece = initPieceTemp
      , nextPieceQueue = List.Nonempty.singleton initPieceTemp
      , heldPiece = Nothing
      , numPiecesGenerated = 1
      , hasAlreadySwapped = False
      , playfield = emptyPlayfield
      , windowSize = windowSize
      }
    , generateNewGamePieces
    )


isLineFilled : Array Space -> Bool
isLineFilled line =
    List.all isSpaceFilled (Array.toList line)


isSpaceFilled : Space -> Bool
isSpaceFilled space =
    space /= emptySpace


isSpaceEmpty : Space -> Bool
isSpaceEmpty space =
    not (isSpaceFilled space)


clearLines : Playfield -> Playfield
clearLines playfield =
    let
        playfieldSubsetWithClearedLines =
            Array.filter (\line -> not (isLineFilled line)) playfield

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
            Rotate Clockwise

        " " ->
            -- Space key
            HardDrop

        "z" ->
            Rotate CounterClockwise

        "Z" ->
            -- Catch lower- or upper-case Z, just in case Caps Lock is on.
            Rotate CounterClockwise

        "a" ->
            Rotate Flip180

        "A" ->
            Rotate Flip180

        "x" ->
            Swap

        "X" ->
            Swap

        _ ->
            NoOp


windowResizeListener : Int -> Int -> Msg
windowResizeListener width height =
    GotResizedWindow (classifyWindowSize { width = width, height = height })


decodeWindowFlags : Json.Decode.Value -> WindowSize
decodeWindowFlags flags =
    case Json.Decode.decodeValue windowDecoder flags of
        Ok window ->
            classifyWindowSize window

        Err _ ->
            Mobile


windowDecoder : Json.Decode.Decoder Window
windowDecoder =
    Json.Decode.map2 Window
        (Json.Decode.field "windowWidth" Json.Decode.int)
        (Json.Decode.field "windowHeight" Json.Decode.int)


{-| Breakpoints taken from [Tailwind](https://tailwindcss.com/docs/responsive-design#overview).
-}
classifyWindowSize : Window -> WindowSize
classifyWindowSize window =
    if window.width >= 1536 then
        ExtraExtraLarge

    else if window.width >= 1280 then
        ExtraLarge

    else if window.width >= 1024 then
        Large

    else if window.width >= 768 then
        Medium

    else if window.width >= 640 then
        Small

    else
        Mobile


getShape : Piece -> Shape
getShape (Piece shape _ _) =
    shape


generateNextPiece : Model -> ( Model, Cmd Msg )
generateNextPiece model =
    ( { model
        | playfield = addPieceToPlayfield (List.Nonempty.head model.nextPieceQueue) (clearLines model.playfield)
        , activePiece = List.Nonempty.head model.nextPieceQueue
        , nextPieceQueue = List.Nonempty.pop model.nextPieceQueue
      }
    , maybeQueueMorePieces model.numPiecesGenerated
    )


{-| Lazily generates [a random "bag"](https://harddrop.com/wiki/Bag) of pieces every so often,
and appends them to the nextPieceQueue to prevent the well from running dry.
-}
maybeQueueMorePieces : Int -> Cmd Msg
maybeQueueMorePieces numPiecesGenerated =
    if modBy (List.length allShapes) (numPiecesGenerated - 1) == 0 then
        generateBag

    else
        Cmd.none


popPieceTemp : List Piece -> ( Piece, List Piece )
popPieceTemp pieces =
    case pieces of
        [] ->
            ( initPieceTemp, [] )

        head :: tail ->
            ( head, tail )


randomBagHelper : Random.Generator (Nonempty Piece)
randomBagHelper =
    Random.List.shuffle (List.map buildPiece allShapes)
        |> Random.map piecesToNonempty


generateBag : Cmd Msg
generateBag =
    Random.generate GenerateBag randomBagHelper


newGameHelper : Random.Generator (Nonempty Piece)
newGameHelper =
    Random.map2 List.Nonempty.append randomBagHelper randomBagHelper


piecesToNonempty : List Piece -> Nonempty Piece
piecesToNonempty pieces =
    pieces
        |> List.Nonempty.fromList
        |> Maybe.withDefault (Nonempty (buildPiece I) (List.map buildPiece (List.drop 1 allShapes)))


generateNewGamePieces : Cmd Msg
generateNewGamePieces =
    Random.generate NewGame newGameHelper


allShapes : List Shape
allShapes =
    [ I, O, T, S, Z, J, L ]


initPieceTemp : Piece
initPieceTemp =
    buildPiece O


buildPiece : Shape -> Piece
buildPiece shape =
    Piece shape (initialPosition shape) initialRotationState


initialRotationState : RotationState
initialRotationState =
    Rotated0


initialPosition : Shape -> Position
initialPosition shape =
    initialPositions shape initialRotationState


{-| Contains "starting positions" of all piece shapes, at all of their rotation states.
The "starting position" is located around the top-middle of the playfield,
and happens to be where a piece is first spawned (at RotationState = Rotation0).
-}
initialPositions : Shape -> RotationState -> Position
initialPositions shape rotationState =
    case ( shape, rotationState ) of
        ( I, Rotated0 ) ->
            buildPosition ( 3, 0 ) ( 4, 0 ) ( 5, 0 ) ( 6, 0 )

        ( I, Rotated90 ) ->
            buildPosition ( 5, -1 ) ( 5, 0 ) ( 5, 1 ) ( 5, 2 )

        ( I, Rotated180 ) ->
            buildPosition ( 3, 1 ) ( 4, 1 ) ( 5, 1 ) ( 6, 1 )

        ( I, Rotated270 ) ->
            buildPosition ( 4, -1 ) ( 4, 0 ) ( 4, 1 ) ( 4, 2 )

        ( O, _ ) ->
            buildPosition ( 3, 0 ) ( 4, 0 ) ( 4, 1 ) ( 3, 1 )

        ( T, Rotated0 ) ->
            buildPosition ( 3, 1 ) ( 4, 1 ) ( 4, 0 ) ( 5, 1 )

        ( T, Rotated90 ) ->
            buildPosition ( 4, 0 ) ( 4, 1 ) ( 5, 1 ) ( 4, 2 )

        ( T, Rotated180 ) ->
            buildPosition ( 5, 1 ) ( 4, 1 ) ( 4, 2 ) ( 3, 1 )

        ( T, Rotated270 ) ->
            buildPosition ( 4, 2 ) ( 4, 1 ) ( 3, 1 ) ( 4, 0 )

        ( S, Rotated0 ) ->
            buildPosition ( 3, 1 ) ( 4, 1 ) ( 4, 0 ) ( 5, 0 )

        ( S, Rotated90 ) ->
            buildPosition ( 4, 0 ) ( 4, 1 ) ( 5, 1 ) ( 5, 2 )

        ( S, Rotated180 ) ->
            buildPosition ( 5, 1 ) ( 4, 1 ) ( 4, 2 ) ( 3, 2 )

        ( S, Rotated270 ) ->
            buildPosition ( 4, 2 ) ( 4, 1 ) ( 3, 1 ) ( 3, 0 )

        ( Z, Rotated0 ) ->
            buildPosition ( 3, 0 ) ( 4, 0 ) ( 4, 1 ) ( 5, 1 )

        ( Z, Rotated90 ) ->
            buildPosition ( 5, 0 ) ( 5, 1 ) ( 4, 1 ) ( 4, 2 )

        ( Z, Rotated180 ) ->
            buildPosition ( 5, 2 ) ( 4, 2 ) ( 4, 1 ) ( 3, 1 )

        ( Z, Rotated270 ) ->
            buildPosition ( 3, 2 ) ( 3, 1 ) ( 4, 1 ) ( 4, 0 )

        ( J, Rotated0 ) ->
            buildPosition ( 3, 0 ) ( 3, 1 ) ( 4, 1 ) ( 5, 1 )

        ( J, Rotated90 ) ->
            buildPosition ( 5, 0 ) ( 4, 0 ) ( 4, 1 ) ( 4, 2 )

        ( J, Rotated180 ) ->
            buildPosition ( 5, 2 ) ( 5, 1 ) ( 4, 1 ) ( 3, 1 )

        ( J, Rotated270 ) ->
            buildPosition ( 3, 2 ) ( 4, 2 ) ( 4, 1 ) ( 4, 0 )

        ( L, Rotated0 ) ->
            buildPosition ( 3, 1 ) ( 4, 1 ) ( 5, 1 ) ( 5, 0 )

        ( L, Rotated90 ) ->
            buildPosition ( 4, 0 ) ( 4, 1 ) ( 4, 2 ) ( 5, 2 )

        ( L, Rotated180 ) ->
            buildPosition ( 5, 1 ) ( 4, 1 ) ( 3, 1 ) ( 3, 2 )

        ( L, Rotated270 ) ->
            buildPosition ( 4, 2 ) ( 4, 1 ) ( 4, 0 ) ( 3, 0 )


buildPosition : ( Int, Int ) -> ( Int, Int ) -> ( Int, Int ) -> ( Int, Int ) -> Position
buildPosition p1 p2 p3 p4 =
    { point1 = p1
    , point2 = p2
    , point3 = p3
    , point4 = p4
    }


emptyPlayfield : Playfield
emptyPlayfield =
    Array.repeat (downLimit + 1) emptyLine


emptyLine : Array Space
emptyLine =
    Array.repeat (rightLimit + 1) emptySpace


pieceToSpace : Piece -> Space
pieceToSpace (Piece shape _ _) =
    Filled shape


emptySpace : Space
emptySpace =
    Empty


addPieceToPlayfield : Piece -> Playfield -> Playfield
addPieceToPlayfield piece playfield =
    updatePieceOnPlayfield (pieceToSpace piece) piece playfield


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



-- VIEW


view : Model -> Html Msg
view model =
    div [ class (.mainClass (getStyleConfig model.windowSize)) ]
        [ showHeldPiece model.windowSize model.heldPiece
        , showPlayfield model.windowSize model.playfield
        , showNextPiece model.windowSize (List.Nonempty.head model.nextPieceQueue)
        , showControls model.windowSize
        ]


showHeldPiece : WindowSize -> Maybe Piece -> Html Msg
showHeldPiece windowSize piece =
    let
        preview =
            case piece of
                Nothing ->
                    text ""

                Just piece_ ->
                    showMiniPiece windowSize (getShape piece_)
    in
    div [ class (.previewClass (getStyleConfig windowSize)), class "piece-preview--hold" ]
        [ text "hold"
        , preview
        ]


showPlayfield : WindowSize -> Playfield -> Html Msg
showPlayfield size playfield =
    div [ class "playfield" ] (showLines size playfield)


showLines : WindowSize -> Playfield -> List (Html Msg)
showLines size playfield =
    Array.toList (Array.map (showLine size) playfield)


showLine : WindowSize -> Array Space -> Html Msg
showLine size line =
    div [ class (.lineClass (getStyleConfig size)) ]
        (Array.toList (Array.map (showSpace size) line))


showSpace : WindowSize -> Space -> Html Msg
showSpace size space =
    let
        blockHeight =
            .blockHeight (getStyleConfig size)

        lineHeight =
            blockHeight + 2

        ( b, l ) =
            ( String.fromInt blockHeight, String.fromInt lineHeight )
    in
    Svg.svg [ width l, height l, viewBox (String.join " " [ "0", "0", l, l ]) ]
        [ Svg.rect [ x "1", y "1", width b, height b, fill (spaceToColor space), stroke "#757575", strokeWidth "1" ] []
        ]


type alias StyleConfig =
    { mainClass : String
    , lineClass : String
    , previewClass : String
    , blockHeight : Int
    , miniBlockHeight : Int
    }


getStyleConfig : WindowSize -> StyleConfig
getStyleConfig size =
    case size of
        Mobile ->
            { mainClass = "main", lineClass = "line", previewClass = "piece-preview", blockHeight = 20, miniBlockHeight = 10 }

        _ ->
            { mainClass = "main main--sm", lineClass = "line line--sm", previewClass = "piece-preview piece-preview--sm", blockHeight = 20, miniBlockHeight = 15 }


spaceToColor : Space -> String
spaceToColor space =
    case space of
        Filled I ->
            -- Cyan
            "#4DD0E1"

        Filled O ->
            -- Yellow
            "#FFF176"

        Filled T ->
            -- Purple
            "#BA68C8"

        Filled S ->
            -- Green
            "#81C784"

        Filled Z ->
            -- Pink
            "#F06292"

        Filled J ->
            -- Blue
            "#0D47A1"

        Filled L ->
            -- Orange
            "#E65100"

        Empty ->
            -- Gray
            "#212121"


showNextPiece : WindowSize -> Piece -> Html Msg
showNextPiece size piece =
    div [ class (.previewClass (getStyleConfig size)), class "piece-preview--next" ]
        [ text "next"
        , showMiniPiece size (getShape piece)
        ]


showControls : WindowSize -> Html Msg
showControls windowSize =
    if windowSize == Mobile then
        showMobileControls

    else
        showKeyboardControls


showMobileControls : Html Msg
showMobileControls =
    div [ class "controls controls--mobile" ]
        [ showDirectionalButtons
        , showActionButtons
        ]


showDirectionalButtons : Html Msg
showDirectionalButtons =
    div [ class "directional-buttons" ]
        [ button [ onClick (Rotate Clockwise), id "button-up" ] [ text "^" ]
        , button [ onClick MoveRight, id "button-right" ] [ text ">" ]
        , button [ onClick SoftDrop, id "button-down" ] [ text "v" ]
        , button [ onClick MoveLeft, id "button-left" ] [ text "<" ]
        ]


showActionButtons : Html Msg
showActionButtons =
    div [ class "action-buttons" ]
        [ button [ onClick Swap ] [ text "Swap" ]
        , button [ onClick HardDrop ] [ text "Hard Drop" ]
        ]


showKeyboardControls : Html Msg
showKeyboardControls =
    let
        ( keys, descriptions ) =
            List.unzip keyControls
    in
    div [ class "controls controls--keyboard" ]
        [ showKeys keys
        , showDescriptions descriptions
        ]


keyControls : List ( String, String )
keyControls =
    [ ( "left", "move left" )
    , ( "right", "move right" )
    , ( "down", "soft drop" )
    , ( "space", "hard drop" )
    , ( "up", "rotate clockwise" )
    , ( "z", "rotate counterclockwise" )
    , ( "a", "rotate 180°" )
    , ( "x", "swap" )
    ]


showKeys : List String -> Html Msg
showKeys keys =
    ul [ class "keys" ]
        (List.map showKey keys)


showKey : String -> Html Msg
showKey key =
    li [] [ strong [] [ text key ] ]


showDescriptions : List String -> Html Msg
showDescriptions descriptions =
    ul [ class "descriptions" ]
        (List.map showDescription descriptions)


showDescription : String -> Html Msg
showDescription description =
    li [] [ text description ]


showMiniPiece : WindowSize -> Shape -> Html Msg
showMiniPiece size shape =
    let
        miniBlockHeight =
            .miniBlockHeight (getStyleConfig size)

        viewBoxWidth =
            miniBlockHeight * 4 + 10

        viewBoxHeight =
            miniBlockHeight * 2 + 10

        ( h, vW, vH ) =
            ( String.fromInt miniBlockHeight, String.fromInt viewBoxWidth, String.fromInt viewBoxHeight )

        buildMiniBlock ( a, b ) =
            Svg.rect [ x (String.fromInt (a * miniBlockHeight + 11)), y (String.fromInt (b * miniBlockHeight + 1)), width h, height h, fill (spaceToColor (Filled shape)), stroke "#212121", strokeWidth "1" ] []

        buildMiniPiece p1 p2 p3 p4 =
            Svg.svg [ width vW, height vH, viewBox (String.join " " [ "0", "0", vW, vH ]) ]
                [ buildMiniBlock p1
                , buildMiniBlock p2
                , buildMiniBlock p3
                , buildMiniBlock p4
                ]
    in
    case shape of
        I ->
            buildMiniPiece ( 0, 0 ) ( 1, 0 ) ( 2, 0 ) ( 3, 0 )

        O ->
            buildMiniPiece ( 0, 0 ) ( 0, 1 ) ( 1, 0 ) ( 1, 1 )

        T ->
            buildMiniPiece ( 0, 1 ) ( 1, 1 ) ( 1, 0 ) ( 2, 1 )

        S ->
            buildMiniPiece ( 0, 1 ) ( 1, 1 ) ( 1, 0 ) ( 2, 0 )

        Z ->
            buildMiniPiece ( 0, 0 ) ( 1, 0 ) ( 1, 1 ) ( 2, 1 )

        J ->
            buildMiniPiece ( 0, 0 ) ( 0, 1 ) ( 1, 1 ) ( 2, 1 )

        L ->
            buildMiniPiece ( 0, 1 ) ( 1, 1 ) ( 2, 1 ) ( 2, 0 )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 1000 Tick
        , Browser.Events.onKeyDown keyDecoder
        , Browser.Events.onResize windowResizeListener
        ]



{-
   TODO Show next 5 pieces
   TODO Either kick tables or T-spins
   TODO Either kick tables or T-spins
   TODO Fix tucks (probably somnething to do with Tick and locking logic)

-}
