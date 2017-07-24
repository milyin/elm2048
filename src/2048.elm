module Elm2048 exposing (Model, Msg, update, view, subscriptions, init)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (..)
import Random
import Styles exposing (..)

{ id, class, classList } = namespace2048

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
    }

type alias Field = {
    array : Array Int,
    height : Int,
    width : Int
}

empty : Int -> Int -> Field
empty w h = {
        array = Array.repeat (h*w) 0,
        height = h,
        width = w
    }

maxx : Field -> Int
maxx field = field.width - 1

maxy : Field -> Int
maxy field = field.height - 1

xs : Field -> List Int
xs field = List.range 0 (maxx field)

ys : Field -> List Int
ys field = List.range 0 (maxy field)

pos2xy : Field -> Int -> (Int,Int)
pos2xy field pos = (pos % field.width, pos // field.width)

xy2pos : Field -> Int -> Int -> Int
xy2pos field x y = y*field.width + x

put : Int -> Int -> Int -> Field -> Field
put val x y field = { field | array = Array.set (xy2pos field x y) val field.array }

drop : Int -> Int -> Int -> Field -> Maybe Field
drop val x y field =
    case (get x y field) of
        0 -> Just (put val x y field)
        _ -> Nothing

hole : Field -> Bool
hole field = Array.foldr (\n hole -> hole || n==0) False field.array

get : Int -> Int -> Field -> Int
get x y field = 
  case Array.get (xy2pos field x y) field.array of
    Just v -> v
    Nothing -> -1

rshrink : List Int -> List Int
rshrink ns = let
        merge n a = case (List.head a) of
            Just h -> if h == n 
                then [0,h+n]++(List.drop 1 a)
                else [n]++a
            Nothing -> [n]
    in
        List.foldr merge [] ns |> List.filter ((/=)0)

hslice : Field -> Int -> List Int
hslice field y = List.map (\x -> get x y field) (xs field) |> List.filter ((/=)0) 

transpose : Field -> Field
transpose field = let
        newfield = {
            width = field.height,
            height = field.width,
            array = Array.empty
        }
        transposer newpos = let
            (x,y) = pos2xy newfield newpos
        in
            get y x field
    in
        { newfield | array = Array.initialize (field.width*field.height) transposer }

type Orientation = Vert | Hor

type Direction = Forward | Backward

doTranspose orient = case orient of
    Vert -> transpose
    Hor -> \x->x

doShrink direct = case direct of
    Forward -> rshrink
    Backward -> (List.reverse >> rshrink >> List.reverse)

doPad w direct slice = let
        zeros slice = List.repeat (w - (List.length slice)) 0
    in case direct of
        Forward -> (zeros slice)++slice
        Backward -> slice++(zeros slice)


shift : Orientation -> Direction -> Field -> Field
shift orient direct field = let
        tfield = doTranspose orient field
        slices = List.map ((hslice tfield) >> (doShrink direct) >> (doPad tfield.width direct)) (ys tfield)
    in
        doTranspose orient { tfield | array = Array.fromList (List.concat slices) }
    

render : Field -> Html msg
render field = let
        renderRow y = div [] (List.map (\x -> renderTile x y) (xs field))
        renderTile x y = let 
                n = get x y field
            in
                div [class (tileClass n)] [ text (if n > 0 then toString n else "") ]
    in
        pre [] (List.map (\y -> renderRow y) (ys field))

type alias Model =
    { score : Int
    , field: Field
    }


type Msg
    = Up
    | Down
    | Left
    | Right
    | Drop
    | Rand Int Int Int


rand field = 
    Random.generate 
        (\(is2,(x,y)) -> Rand (if is2 then 2 else 4) x y) 
        (Random.pair Random.bool (Random.pair (Random.int 0 (maxx field)) (Random.int 0 (maxy field))))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Up ->
            (Model 0 (shift Vert Backward model.field), Cmd.none)
        Down ->
            (Model 0 (shift Vert Forward model.field), Cmd.none)
        Left ->
            (Model 0 (shift Hor Backward model.field), Cmd.none)
        Right ->
            (Model 0 (shift Hor Forward model.field), Cmd.none)
        Drop ->
            if hole model.field
                then (model, rand model.field)
                else (Model -1 model.field, Cmd.none)
        Rand v x y -> 
            case (drop v x y model.field) of
                Just field -> (Model 0 field, Cmd.none)
                Nothing -> (model, rand model.field)


view : Model -> Html Msg
view model =
    div []
        [ text (toString model.score)
        , br [] []
        , (text <| toString <| Array.length model.field.array)
        , br [] []
        , render model.field
        , button [onClick Up] [text "Up"]
        , button [onClick Down] [text "Down"]
        , button [onClick Left] [text "Left"]
        , button [onClick Right] [text "Right"]
        , button [onClick Drop] [text "Drop"]
        ]

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

init : (Model, Cmd Msg)
init = 
    ( Model 0 (empty 4 4)
    , Cmd.none
    
    )
