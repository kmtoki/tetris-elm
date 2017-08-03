module Tetris exposing (..)

import Array exposing (Array)
import List
import Tuple
import Debug


type Color
    = Red
    | Blue
    | Green
    | Yellow
    | Purple
    | Cyan
    | White


type alias Size =
    { width : Int
    , height : Int
    }


type alias Pos =
    { x : Int
    , y : Int
    }


type alias Field =
    Array (Array (Maybe Color))


type alias Block =
    { color : Color
    , vertex : List Pos
    }


type alias Tetris =
    { size : Size
    , field : Field
    , block : Block
    }


init : Int -> Int -> Tetris
init w h =
    { size = Size w h
    , field = Array.repeat h <| Array.repeat w Nothing
    , block = Block Red []
    }


all : (a -> Bool) -> Array a -> Bool
all f =
    Array.foldl (\a b -> b && f a) True


isJust : Maybe a -> Bool
isJust m =
    case m of
        Nothing ->
            False

        Just _ ->
            True


isNothing : Maybe a -> Bool
isNothing =
    not << isJust


delete : Tetris -> Tetris
delete t =
    let
        f =
            Array.map
                (\xs ->
                    if all isJust xs then
                        Array.map (\_ -> Nothing) xs
                    else
                        xs
                )
                t.field
    in
        { t | field = f }


fall : Tetris -> Tetris
fall t =
    let
        f_ =
            (Array.filter (\xs -> not <| all isNothing xs) t.field)

        f =
            Array.append
                (Array.repeat (Array.length t.field - Array.length f_) <|
                    Array.repeat t.size.width Nothing
                )
                f_
    in
        { t | field = f }


isCollision : Tetris -> Bool
isCollision t =
    let
        f { x, y } =
            case Array.get y t.field of
                Nothing ->
                    True

                Just xs ->
                    case Array.get x xs of
                        Nothing ->
                            True

                        Just m ->
                            case m of
                                Nothing ->
                                    False

                                Just _ ->
                                    True
    in
        List.any f t.block.vertex


rotate : Tetris -> Tetris
rotate t =
    let
        r =
            pi / 2.0

        my =
            toFloat (List.sum <| List.map .y t.block.vertex)
                / toFloat (List.length t.block.vertex)

        mx =
            toFloat (List.sum <| List.map .x t.block.vertex)
                / toFloat (List.length t.block.vertex)

        f { x, y } =
            let
                x_ =
                    toFloat x

                y_ =
                    toFloat y
            in
                Pos
                    (round (mx + (x_ - mx) * cos r - (y_ - my) * sin r))
                    (round (my + (x_ - mx) * sin r + (y_ - my) * cos r))

        t_ =
            { t | block = Block t.block.color (List.map f t.block.vertex) }
    in
        if isCollision t_ then
            t
        else
            t_


left : Tetris -> Tetris
left t =
    let
        f { x, y } =
            Pos (x - 1) y

        t_ =
            { t | block = Block t.block.color (List.map f t.block.vertex) }
    in
        if isCollision t_ then
            t
        else
            t_


right : Tetris -> Tetris
right t =
    let
        f { x, y } =
            Pos (x + 1) y

        t_ =
            { t | block = Block t.block.color (List.map f t.block.vertex) }
    in
        if isCollision t_ then
            t
        else
            t_


down : Tetris -> Maybe Tetris
down t =
    let
        f { x, y } =
            Pos x (y + 1)

        t_ =
            { t | block = Block t.block.color (List.map f t.block.vertex) }
    in
        if isCollision t_ then
            Nothing
        else
            Just t_


writeBlock : Tetris -> Maybe Tetris
writeBlock t =
    let
        f { x, y } m =
            m
                |> Maybe.andThen
                    (\field ->
                        Array.get y field
                            |> Maybe.andThen
                                (\xs ->
                                    Array.get x xs
                                        |> Maybe.andThen
                                            (\m ->
                                                case m of
                                                    Just _ ->
                                                        Nothing

                                                    Nothing ->
                                                        Just
                                                            (Array.set
                                                                y
                                                                (Array.set
                                                                    x
                                                                    (Just t.block.color)
                                                                    xs
                                                                )
                                                                field
                                                            )
                                            )
                                )
                    )
    in
        List.foldl f (Just t.field) t.block.vertex
            |> Maybe.andThen (\field -> Just { t | field = field })


newBlock : Int -> Int -> Tetris -> Tetris
newBlock c b t =
    let
        block =
            case Array.get c colors of
                Nothing ->
                    Block White [ Pos 0 0 ]

                Just c_ ->
                    case Array.get b blocks of
                        Nothing ->
                            Block White [ Pos 0 0 ]

                        Just b_ ->
                            Block c_ b_
    in
        { t | block = block }


score : Tetris -> Tetris -> Int
score t t_ =
    let
        f t =
            List.sum <|
                Array.toList <|
                    Array.map
                        (\xs ->
                            if all isNothing xs then
                                1
                            else
                                0
                        )
                        t.field
    in
        f t_ - f t


blocks : Array (List Pos)
blocks =
    Array.fromList
        [ [ Pos 0 0, Pos 0 1, Pos 0 2, Pos 0 3 ]
        , [ Pos 0 0, Pos 1 0, Pos 1 1, Pos 2 1 ]
        , [ Pos 0 1, Pos 1 1, Pos 1 0, Pos 2 0 ]
        , [ Pos 0 0, Pos 1 0, Pos 0 1, Pos 1 1 ]
        , [ Pos 0 0, Pos 1 0, Pos 2 0, Pos 1 1 ]
        ]


colors : Array Color
colors =
    Array.fromList
        [ Red, Blue, Green, Yellow, Purple, Cyan ]
