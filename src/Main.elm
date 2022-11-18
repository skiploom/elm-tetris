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
   TODO Display controls + any quick UI prettifying
   TODO Deploy to GitHub :D
   TODO Allow first piece to be any piece, not just O
   TODO Counterclockwise rotation
   TODO 180 degree rotation
   TODO Show next piece
   TODO Allow piece swapping/holding
   TODO Fix piece randomizing to be more like Tetris Guideline
   TODO Either kick tables or T-spins
   TODO Either kick tables or T-spins
   TODO Fix tucks (probably somnething to do with Tick and locking logic)

-}
-- MAIN


main =
    Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }



-- MODEL


type alias Model =
    { activePiece : Piece
    , playfield : Playfield
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


type WhichWay
    = Left
    | Right
    | Down


type alias Playfield =
    Array (Array Space)


type Space
    = Filled Shape
    | Empty


init : () -> ( Model, Cmd Msg )
init _ =
    ( { activePiece = initPieceTemp
      , playfield = addPieceToPlayfield initPieceTemp emptyPlayfield
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = MoveLeft
    | MoveRight
    | SoftDrop
    | HardDrop
    | RotateClockwise
    | NewPiece Piece
    | Tick Time.Posix
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        Tick time ->
            maybeLockPiece (maybeRefreshPlayfield model Down)

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
            getPosition (movePiece model.activePiece whichWay)
    in
    isWithinPlayfieldBounds destination && areSpacesEmpty destination model


canPieceRotateThatWay : Model -> Bool
canPieceRotateThatWay model =
    let
        destination =
            getPosition (rotateClockwiseHelper model.activePiece)
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
        { model
            | activePiece = rotateClockwiseHelper model.activePiece
            , playfield = refreshPlayfieldHelper model.activePiece (rotateClockwiseHelper model.activePiece) model.playfield
        }

    else
        model


rotateClockwiseHelper : Piece -> Piece
rotateClockwiseHelper piece =
    piece
        |> setRotationState
        |> rotatePosition


{-| Assume clockwise.
-}
setRotationState : Piece -> Piece
setRotationState (Piece shape position rotationState) =
    Piece shape position (cycleRotationState rotationState)


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


type alias RotationDelta =
    { d1 : ( Int, Int )
    , d2 : ( Int, Int )
    , d3 : ( Int, Int )
    , d4 : ( Int, Int )
    }


{-| Eventually this should handle counterclockwise. Assume clockwise for now.
-}
getRotationDelta : Piece -> RotationDelta
getRotationDelta (Piece shape _ rotationState) =
    case ( shape, rotationState ) of
        ( I, Rotated90 ) ->
            buildRotationDelta ( 2, -1 ) ( 1, 0 ) ( 0, 1 ) ( -1, 2 )

        ( I, Rotated180 ) ->
            buildRotationDelta ( -2, 2 ) ( -1, 1 ) ( 0, 0 ) ( 1, -1 )

        ( I, Rotated270 ) ->
            buildRotationDelta ( 1, -2 ) ( 0, -1 ) ( -1, 0 ) ( -2, 1 )

        ( I, Rotated0 ) ->
            buildRotationDelta ( -1, 1 ) ( 0, 0 ) ( 1, -1 ) ( 2, -2 )

        ( O, _ ) ->
            buildRotationDelta ( 0, 0 ) ( 0, 0 ) ( 0, 0 ) ( 0, 0 )

        ( T, Rotated90 ) ->
            buildRotationDelta ( 1, -1 ) ( 0, 0 ) ( 1, 1 ) ( -1, 1 )

        ( T, Rotated180 ) ->
            buildRotationDelta ( 1, 1 ) ( 0, 0 ) ( -1, 1 ) ( -1, -1 )

        ( T, Rotated270 ) ->
            buildRotationDelta ( -1, 1 ) ( 0, 0 ) ( -1, -1 ) ( 1, -1 )

        ( T, Rotated0 ) ->
            buildRotationDelta ( -1, -1 ) ( 0, 0 ) ( 1, -1 ) ( 1, 1 )

        ( S, Rotated90 ) ->
            buildRotationDelta ( 1, -1 ) ( 0, 0 ) ( 1, 1 ) ( 0, 2 )

        ( S, Rotated180 ) ->
            buildRotationDelta ( 1, 1 ) ( 0, 0 ) ( -1, 1 ) ( -2, 0 )

        ( S, Rotated270 ) ->
            buildRotationDelta ( -1, 1 ) ( 0, 0 ) ( -1, -1 ) ( 0, -2 )

        ( S, Rotated0 ) ->
            buildRotationDelta ( -1, -1 ) ( 0, 0 ) ( 1, -1 ) ( 2, 0 )

        ( Z, Rotated90 ) ->
            buildRotationDelta ( 2, 0 ) ( 1, 1 ) ( 0, 0 ) ( -1, 1 )

        ( Z, Rotated180 ) ->
            buildRotationDelta ( 0, 2 ) ( -1, 1 ) ( 0, 0 ) ( -1, -1 )

        ( Z, Rotated270 ) ->
            buildRotationDelta ( -2, 0 ) ( -1, -1 ) ( 0, 0 ) ( 1, -1 )

        ( Z, Rotated0 ) ->
            buildRotationDelta ( 0, -2 ) ( 1, -1 ) ( 0, 0 ) ( 1, 1 )

        ( J, Rotated90 ) ->
            buildRotationDelta ( 2, 0 ) ( 1, -1 ) ( 0, 0 ) ( -1, 1 )

        ( J, Rotated180 ) ->
            buildRotationDelta ( 0, 2 ) ( 1, 1 ) ( 0, 0 ) ( -1, -1 )

        ( J, Rotated270 ) ->
            buildRotationDelta ( -2, 0 ) ( -1, 1 ) ( 0, 0 ) ( 1, -1 )

        ( J, Rotated0 ) ->
            buildRotationDelta ( 0, -2 ) ( -1, -1 ) ( 0, 0 ) ( 1, 1 )

        ( L, Rotated90 ) ->
            buildRotationDelta ( 1, -1 ) ( 0, 0 ) ( -1, 1 ) ( 0, 2 )

        ( L, Rotated180 ) ->
            buildRotationDelta ( 1, 1 ) ( 0, 0 ) ( -1, -1 ) ( -2, 0 )

        ( L, Rotated270 ) ->
            buildRotationDelta ( -1, 1 ) ( 0, 0 ) ( 1, -1 ) ( 0, -2 )

        ( L, Rotated0 ) ->
            buildRotationDelta ( -1, -1 ) ( 0, 0 ) ( 1, 1 ) ( 2, 0 )


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
    { model | activePiece = movedPiece, playfield = newPlayfield }


{-| Finds how many spaces the active piece can move down before colliding with existing pieces
-}
hardDropHelper : Model -> Bool -> Int -> Int
hardDropHelper model shouldContinue counter =
    case shouldContinue of
        True ->
            hardDropHelper { model | activePiece = movePiece model.activePiece Down } (not (isPieceStuck model)) (counter + 1)

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
        ( { activePiece = initPieceTemp
          , playfield = addPieceToPlayfield initPieceTemp emptyPlayfield
          }
        , Cmd.none
        )

    else if isPieceStuck model then
        ( model, newPiece )

    else
        ( model, Cmd.none )


lockPiece : Model -> Model
lockPiece model =
    { model | playfield = addPieceToPlayfield initPieceTemp (clearLines model.playfield), activePiece = initPieceTemp }


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
            RotateClockwise

        " " ->
            -- Space key
            HardDrop

        _ ->
            NoOp


randomPieceHelper : Random.Generator Piece
randomPieceHelper =
    Random.uniform (buildPiece I) (List.map buildPiece [ O, T, S, Z, J, L ])


newPiece : Cmd Msg
newPiece =
    Random.generate NewPiece randomPieceHelper


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
    case shape of
        I ->
            buildPosition ( 3, 0 ) ( 4, 0 ) ( 5, 0 ) ( 6, 0 )

        O ->
            buildPosition ( 3, 0 ) ( 4, 0 ) ( 4, 1 ) ( 3, 1 )

        T ->
            buildPosition ( 3, 2 ) ( 4, 2 ) ( 4, 1 ) ( 5, 2 )

        S ->
            buildPosition ( 3, 1 ) ( 4, 1 ) ( 4, 0 ) ( 5, 0 )

        Z ->
            buildPosition ( 3, 0 ) ( 4, 0 ) ( 4, 1 ) ( 5, 1 )

        J ->
            buildPosition ( 3, 0 ) ( 3, 1 ) ( 4, 1 ) ( 5, 1 )

        L ->
            buildPosition ( 3, 1 ) ( 4, 1 ) ( 5, 1 ) ( 5, 0 )


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
view { playfield, activePiece } =
    div []
        [ showPlayfield playfield
        ]


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
    span [ style "color" (spaceToColor space) ] [ text (showBlock space) ]


showBlock : Space -> String
showBlock space =
    if space == emptySpace then
        "□"

    else
        "▣"


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
            "#757575"


pieceToSpace : Piece -> Space
pieceToSpace (Piece shape _ _) =
    Filled shape


emptySpace : Space
emptySpace =
    Empty



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 1000 Tick
        , Browser.Events.onKeyDown keyDecoder
        ]
