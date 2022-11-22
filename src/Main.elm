module Main exposing (..)

import Array exposing (Array)
import Browser
import Browser.Events
import Html exposing (Html, br, button, div, li, span, strong, text, ul)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Json.Decode
import List.Extra
import Random
import Svg
import Svg.Attributes exposing (fill, height, rx, ry, stroke, strokeWidth, viewBox, width, x, y)
import Time



-- MAIN


main =
    Browser.element { init = init, subscriptions = subscriptions, update = update, view = view }



-- MODEL


type alias Model =
    { activePiece : Piece
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
    = MoveLeft
    | MoveRight
    | SoftDrop
    | HardDrop
    | RotateClockwise
    | RotateCounterClockwise
    | NewPiece Piece
    | Tick Time.Posix
    | GotResizedWindow WindowSize
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
            ( rotate Clockwise model, Cmd.none )

        RotateCounterClockwise ->
            ( rotate CounterClockwise model, Cmd.none )

        NewPiece piece ->
            ( { model | playfield = addPieceToPlayfield piece (clearLines model.playfield), activePiece = piece }, Cmd.none )

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
        newGame model.windowSize

    else if isPieceStuck model then
        ( model, newPiece )

    else
        ( model, Cmd.none )


newGame : WindowSize -> ( Model, Cmd Msg )
newGame windowSize =
    ( { activePiece = initPieceTemp
      , playfield = emptyPlayfield
      , windowSize = windowSize
      }
    , newPiece
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
            RotateClockwise

        " " ->
            -- Space key
            HardDrop

        "z" ->
            RotateCounterClockwise

        "Z" ->
            -- Catch lower- or upper-case Z, just in case Caps Lock is on.
            RotateCounterClockwise

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
            buildPosition ( 3, 2 ) ( 4, 2 ) ( 4, 1 ) ( 5, 2 )

        ( T, Rotated90 ) ->
            buildPosition ( 4, 1 ) ( 4, 2 ) ( 5, 2 ) ( 4, 3 )

        ( T, Rotated180 ) ->
            buildPosition ( 5, 2 ) ( 4, 2 ) ( 4, 3 ) ( 3, 2 )

        ( T, Rotated270 ) ->
            buildPosition ( 4, 3 ) ( 4, 2 ) ( 3, 2 ) ( 4, 1 )

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
    div [ style "display" "flex", style "flex-direction" "column", style "align-items" "center" ]
        [ showGame model
        , showControls model.windowSize
        ]


showGame : Model -> Html Msg
showGame model =
    if model.windowSize == Mobile then
        showPlayfield showBigSpace model.playfield

    else
        showPlayfield showSpace model.playfield


showPlayfield : (Space -> Html Msg) -> Playfield -> Html Msg
showPlayfield showSpaceFn playfield =
    div [] (Array.toList (Array.map (showLine showSpaceFn) playfield))


showLine : (Space -> Html Msg) -> Array Space -> Html Msg
showLine showSpaceFn line =
    div [] (Array.toList (Array.map showSpaceFn line))


showSpace : Space -> Html Msg
showSpace space =
    Svg.svg [ width "22", height "22", viewBox "0 0 22 22" ]
        [ Svg.rect [ x "1", y "1", width "20", height "20", fill (spaceToColor space), stroke "#757575", strokeWidth "1" ] []
        ]


showBigSpace : Space -> Html Msg
showBigSpace space =
    Svg.svg [ width "30", height "30", viewBox "0 0 30 30" ]
        [ Svg.rect [ x "1", y "1", width "28", height "28", fill (spaceToColor space), stroke "#757575", strokeWidth "1" ] []
        ]


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


showControls : WindowSize -> Html Msg
showControls windowSize =
    if windowSize == Mobile then
        showMobileControls

    else
        showKeyboardControls


showMobileControls : Html Msg
showMobileControls =
    div [ style "padding-top" "10px", style "font-family" "monospace", style "display" "grid", style "grid-template-columns" "180px auto 120px" ]
        [ showDirectionalButtons
        , div [] [ text "" ]
        , showActionButtons
        ]


showDirectionalButtons : Html Msg
showDirectionalButtons =
    div
        [ style "display" "grid"
        , style "grid-template-columns" "repeat(3, 50px [col-start])"
        , style "grid-template-rows" "repeat(3, 50px [col-start])"
        ]
        [ div [] [ text "" ]
        , button (onClick RotateClockwise :: buttonColorAttrs) [ text "^" ]
        , div [] [ text "" ]
        , button (onClick MoveLeft :: buttonColorAttrs) [ text "<" ]
        , div [] [ text "" ]
        , button (onClick MoveRight :: buttonColorAttrs) [ text ">" ]
        , div [] [ text "" ]
        , button (onClick SoftDrop :: buttonColorAttrs) [ text "v" ]
        , div [] [ text "" ]
        ]


showActionButtons : Html Msg
showActionButtons =
    div [ style "display" "flex", style "flex-direction" "column", style "justify-content" "space-evenly" ]
        [ button ([ onClick RotateClockwise, style "height" "40px" ] ++ buttonColorAttrs) [ text "Rotate Clockwise" ]
        , button ([ onClick RotateCounterClockwise, style "height" "40px" ] ++ buttonColorAttrs) [ text "Rotate CCW" ]
        , button ([ onClick HardDrop, style "height" "40px" ] ++ buttonColorAttrs) [ text "Hard Drop" ]
        ]


buttonColorAttrs : List (Html.Attribute Msg)
buttonColorAttrs =
    [ style "border" "0", style "border-radius" "5px", style "background-color" "#AED581", style "color" "white" ]


showKeyboardControls : Html Msg
showKeyboardControls =
    let
        ( keys, descriptions ) =
            List.unzip keyControls
    in
    div [ style "font-family" "monospace", style "display" "flex" ]
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
    ]


showKeys : List String -> Html Msg
showKeys keys =
    ul [ style "display" "flex", style "flex-direction" "column", style "align-items" "flex-start", style "padding" "0", style "list-style-type" "none", style "color" "#AED581" ]
        (List.map showKey keys)


showKey : String -> Html Msg
showKey key =
    li [] [ strong [] [ text key ] ]


showDescriptions : List String -> Html Msg
showDescriptions descriptions =
    ul [ style "display" "flex", style "flex-direction" "column", style "align-items" "flex-end", style "list-style-type" "none", style "color" "#E0E0E0" ]
        (List.map showDescription descriptions)


showDescription : String -> Html Msg
showDescription description =
    li [] [ text description ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 1000 Tick
        , Browser.Events.onKeyDown keyDecoder
        , Browser.Events.onResize windowResizeListener
        ]



{-
   TODO 180 degree rotation
   TODO Show next piece
   TODO Allow piece swapping/holding
   TODO Clean up code and pretty up mobile UI
   TODO Fix piece randomizing to be more like Tetris Guideline
   TODO Either kick tables or T-spins
   TODO Either kick tables or T-spins
   TODO Fix tucks (probably somnething to do with Tick and locking logic)

-}
