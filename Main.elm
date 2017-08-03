module Main exposing (..)

import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Svg as S
import Svg.Attributes as SA
import Keyboard
import Array
import Char
import Debug
import Random
import Time
import Tetris exposing (Tetris)


main =
    H.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { tetris : Tetris
    , score : Int
    , next : Tetris.Block
    , isGameOver : Bool
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            { tetris = Tetris.init 10 20
            , score = 0
            , next = Tetris.Block Tetris.Red [ Tetris.Pos 0 0 ]
            , isGameOver = False
            }

        ( model_, cmd ) =
            update RandomBlock model

        ( model__, cmd_ ) =
            update RandomBlock model_
    in
        model__ ! [ cmd, cmd_ ]


type Msg
    = Init
    | GameOver
    | NewBlock Int Int
    | WriteBlock
    | RandomBlock
    | Rotate
    | Left
    | Right
    | Down


controller : Char.KeyCode -> Msg
controller kc =
    case kc of
        37 ->
            Left

        38 ->
            Rotate

        39 ->
            Right

        40 ->
            Down

        _ ->
            Down


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every Time.second (always Down)
        , Keyboard.downs controller
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Init ->
            init

        GameOver ->
            ( { model | isGameOver = True }, Cmd.none )

        NewBlock c b ->
            ( { model
                | next = (Tetris.newBlock c b model.tetris).block
                , tetris =
                    { size = model.tetris.size
                    , field = model.tetris.field
                    , block = model.next
                    }
              }
            , Cmd.none
            )

        WriteBlock ->
            case Tetris.writeBlock model.tetris of
                Nothing ->
                    update GameOver model

                Just t ->
                    update RandomBlock { model | tetris = t }

        RandomBlock ->
            let
                rb =
                    Random.int 0 (Array.length Tetris.blocks - 1)

                rc =
                    Random.int 0 (Array.length Tetris.colors - 1)

                newBlock =
                    Random.generate identity <| Random.map2 NewBlock rc rb
            in
                ( model, newBlock )

        Rotate ->
            ( { model | tetris = Tetris.rotate model.tetris }, Cmd.none )

        Left ->
            ( { model | tetris = Tetris.left model.tetris }, Cmd.none )

        Right ->
            ( { model | tetris = Tetris.right model.tetris }, Cmd.none )

        Down ->
            case Tetris.down model.tetris of
                Nothing ->
                    update WriteBlock model

                Just t ->
                    let
                        t_ =
                            Tetris.delete t

                        s =
                            model.score + Tetris.score t t_
                    in
                        ( { model | score = s, tetris = Tetris.fall t_ }, Cmd.none )


view : Model -> H.Html Msg
view model =
    H.div
        []
        [ viewInit
        , viewScore model.score
        , viewNext model.next
        , viewTetris model.tetris
        , viewGameOver model.isGameOver
        ]


viewInit =
    H.div
        [ HA.style [ ( "position", "absolute" ), ( "top", "400px" ), ( "left", "300px" ) ] ]
        [ H.button [ HE.onClick Init ] [ H.text "reset" ] ]


rect x y c =
    S.rect
        [ SA.width "25"
        , SA.height "25"
        , SA.x (toString <| x * 25)
        , SA.y (toString <| y * 25)
        , SA.fill c
        , SA.stroke "grey"
        ]
        []


viewScore s =
    H.div
        [ HA.style [ ( "position", "absolute" ), ( "top", "300px" ), ( "left", "300px" ) ] ]
        [ S.text <| ("Score: " ++ toString s) ]


eqPos : Int -> Int -> Tetris.Pos -> Bool
eqPos x_ y_ { x, y } =
    x == x_ && y == y_


viewNext b =
    H.div
        [ HA.style [ ( "position", "absolute" ), ( "top", "25px" ), ( "left", "300px" ) ] ]
        [ S.svg [ SA.width "100", SA.height "100" ]
            (List.repeat 4 "black"
                |> List.repeat 4
                |> List.indexedMap
                    (\y xs ->
                        xs
                            |> List.indexedMap
                                (\x c ->
                                    rect x
                                        y
                                        (if List.any (eqPos x y) b.vertex then
                                            toString b.color
                                         else
                                            c
                                        )
                                )
                    )
                |> List.concat
            )
        ]


viewTetris t =
    H.div
        [ HA.style [ ( "position", "absolute" ), ( "top", "25px" ), ( "left", "10px" ) ] ]
        [ S.svg [ SA.width "250", SA.height "500" ]
            (t.field
                |> Array.indexedMap
                    (\y xs ->
                        xs
                            |> Array.indexedMap
                                (\x m ->
                                    rect x
                                        y
                                        (if List.any (eqPos x y) t.block.vertex then
                                            toString t.block.color
                                         else
                                            case m of
                                                Nothing ->
                                                    "black"

                                                Just c ->
                                                    toString c
                                        )
                                )
                            |> Array.toList
                    )
                |> Array.toList
                |> List.concat
            )
        ]


viewGameOver bool =
    if bool then
        H.div
            [ HA.style
                [ ( "position", "absolute" )
                , ( "top", "350px" )
                , ( "left", "300px" )
                , ( "color", "red" )
                ]
            ]
            [ H.text "GameOver" ]
    else
        H.div [] []
