port module Main exposing (main)

import Array exposing (Array)
import Bitwise
import Browser.Events as Events
import Json.Decode as Decode exposing (Decoder)
import Platform


port render : List (List Int) -> Cmd msg


screenWidth : number
screenWidth =
    256


screenHeight : number
screenHeight =
    144


planeX : Float
planeX =
    0


planeY : Float
planeY =
    0.66


type Cell
    = Empty
    | Wall


map : Array (Array Cell)
map =
    let
        ( e, w ) =
            ( Empty, Wall )
    in
    [ [ e, e, e, e, e, e, e, e ]
    , [ e, e, e, e, e, w, w, e ]
    , [ e, e, e, e, e, w, e, e ]
    , [ w, w, e, w, e, e, w, e ]
    , [ e, w, e, w, e, e, e, e ]
    , [ e, w, e, w, w, w, w, e ]
    , [ e, w, e, e, e, w, e, e ]
    , [ e, e, e, e, e, w, e, e ]
    ]
        |> List.map Array.fromList
        |> Array.fromList


cellAt : Int -> Int -> Cell
cellAt x y =
    Array.get y map
        |> Maybe.andThen (Array.get x)
        |> Maybe.withDefault Wall


type alias Model =
    { goingForward : Bool
    , goingBackward : Bool
    , rotatingLeft : Bool
    , rotatingRight : Bool
    , posX : Float
    , posY : Float
    , angle : Float
    }


type Msg
    = FramePassed Float
    | KeyPressed Key
    | KeyReleased Key


type Key
    = Up
    | Down
    | Left
    | Right


keyDecoder : Decoder Key
keyDecoder =
    Decode.field "key" Decode.string
        |> Decode.andThen
            (\s ->
                case s of
                    "ArrowUp" ->
                        Decode.succeed Up

                    "ArrowDown" ->
                        Decode.succeed Down

                    "ArrowLeft" ->
                        Decode.succeed Left

                    "ArrowRight" ->
                        Decode.succeed Right

                    _ ->
                        Decode.fail ("Not interested in " ++ s)
            )


main : Program () Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { goingForward = False
      , goingBackward = False
      , rotatingLeft = False
      , rotatingRight = False
      , posX = 1.5
      , posY = 1.5
      , angle = pi / 2
      }
    , Cmd.none
    )


raycasting : Model -> List (List Int)
raycasting model =
    let
        ( dirX, dirY ) =
            ( cos model.angle, sin model.angle )

        ( rotPlaneX, rotPlaneY ) =
            ( (planeX * cos model.angle) - (planeY * sin model.angle)
            , (planeX * sin model.angle) + (planeY * cos model.angle)
            )

        lineProperties x =
            let
                cameraX =
                    2 * toFloat x / toFloat screenWidth - 1

                ( rayDirX, rayDirY ) =
                    ( dirX + rotPlaneX * cameraX, dirY + rotPlaneY * cameraX )

                ( mapX, mapY ) =
                    ( floor model.posX, floor model.posY )

                ( deltaDistX, deltaDistY ) =
                    ( abs (1 / rayDirX), abs (1 / rayDirY) )

                ( stepX, sideDistX ) =
                    if rayDirX < 0 then
                        ( -1, (model.posX - toFloat mapX) * deltaDistX )

                    else
                        ( 1, (toFloat mapX + 1 - model.posX) * deltaDistX )

                ( stepY, sideDistY ) =
                    if rayDirY < 0 then
                        ( -1, (model.posY - toFloat mapY) * deltaDistY )

                    else
                        ( 1, (toFloat mapY + 1 - model.posY) * deltaDistY )

                dda prevSideDistX prevSideDistY prevMapX prevMapY =
                    let
                        ( ( newSideDistX, newSideDistY ), ( newMapX, newMapY ), side ) =
                            if prevSideDistX < prevSideDistY then
                                ( ( prevSideDistX + deltaDistX, prevSideDistY )
                                , ( prevMapX + stepX, prevMapY )
                                , 0
                                )

                            else
                                ( ( prevSideDistX, prevSideDistY + deltaDistY )
                                , ( prevMapX, prevMapY + stepY )
                                , 1
                                )
                    in
                    if cellAt newMapX newMapY == Wall then
                        ( newMapX, newMapY, side )

                    else
                        dda newSideDistX newSideDistY newMapX newMapY

                ( hitMapX, hitMapY, hitSide ) =
                    dda sideDistX sideDistY mapX mapY

                ( perpWallDist, ( r, g, b ) ) =
                    if hitSide == 0 then
                        ( (toFloat hitMapX - model.posX + (1 - stepX) / 2) / rayDirX
                        , ( 0, 0, 128 )
                        )

                    else
                        ( (toFloat hitMapY - model.posY + (1 - stepY) / 2) / rayDirY
                        , ( 0, 0, 255 )
                        )

                shade =
                    0.25 + (0.75 * (min (screenHeight / perpWallDist) screenHeight / screenHeight))

                color =
                    rgb (round (r * shade)) (round (g * shade)) (round (b * shade))
            in
            ( min (round (screenHeight / perpWallDist)) screenHeight
            , color
            )
    in
    List.map
        (\x ->
            let
                ( height, color ) =
                    lineProperties x
            in
            List.repeat height color
                |> pad screenHeight 0
        )
        (List.range 0 (screenWidth - 1))


rgb : Int -> Int -> Int -> Int
rgb r g b =
    Bitwise.and 0xFF b
        |> Bitwise.or (Bitwise.shiftLeftBy 8 (Bitwise.and 0xFF g))
        |> Bitwise.or (Bitwise.shiftLeftBy 16 (Bitwise.and 0xFF r))


pad : Int -> a -> List a -> List a
pad n value list =
    let
        length =
            List.length list

        a =
            (n - length) // 2

        b =
            n - a - length
    in
    List.repeat a value ++ list ++ List.repeat b value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FramePassed delta ->
            let
                newModel =
                    step delta model
            in
            ( newModel, render (raycasting newModel) )

        KeyPressed Up ->
            ( { model | goingForward = True }, Cmd.none )

        KeyReleased Up ->
            ( { model | goingForward = False }, Cmd.none )

        KeyPressed Down ->
            ( { model | goingBackward = True }, Cmd.none )

        KeyReleased Down ->
            ( { model | goingBackward = False }, Cmd.none )

        KeyPressed Left ->
            ( { model | rotatingLeft = True }, Cmd.none )

        KeyReleased Left ->
            ( { model | rotatingLeft = False }, Cmd.none )

        KeyPressed Right ->
            ( { model | rotatingRight = True }, Cmd.none )

        KeyReleased Right ->
            ( { model | rotatingRight = False }, Cmd.none )


step : Float -> Model -> Model
step delta model =
    let
        rotationSpeed =
            0.0025 * delta

        newAngle =
            case ( model.rotatingLeft, model.rotatingRight ) of
                ( True, False ) ->
                    model.angle - rotationSpeed

                ( False, True ) ->
                    model.angle + rotationSpeed

                _ ->
                    model.angle

        movementSpeed =
            0.0025 * delta

        ( newPosX, newPosY ) =
            case ( model.goingForward, model.goingBackward ) of
                ( True, False ) ->
                    attemptMove
                        ( model.posX + cos newAngle * movementSpeed
                        , model.posY + sin newAngle * movementSpeed
                        )
                        ( model.posX
                        , model.posY
                        )

                ( False, True ) ->
                    attemptMove
                        ( model.posX - cos newAngle * movementSpeed
                        , model.posY - sin newAngle * movementSpeed
                        )
                        ( model.posX
                        , model.posY
                        )

                _ ->
                    ( model.posX, model.posY )
    in
    { model | angle = newAngle, posX = newPosX, posY = newPosY }


attemptMove : ( Float, Float ) -> ( Float, Float ) -> ( Float, Float )
attemptMove ( targetPosX, targetPosY ) ( posX, posY ) =
    ( if cellAt (floor targetPosX) (floor posY) == Wall then
        posX

      else
        targetPosX
    , if cellAt (floor posX) (floor targetPosY) == Wall then
        posY

      else
        targetPosY
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onAnimationFrameDelta FramePassed
        , Events.onKeyDown (Decode.map KeyPressed keyDecoder)
        , Events.onKeyUp (Decode.map KeyReleased keyDecoder)
        ]
