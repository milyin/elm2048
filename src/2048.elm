module Elm2048 exposing (Model, Msg, update, view, subscriptions, init)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (..)
import Random
import Styles exposing (..)
import Keyboard exposing (..)
import Mouse exposing (Position)
import Json.Decode as Decode
import TouchEvents exposing (onTouchStart, onTouchEnd, Touch)
import Tuple exposing (..)

{ id, class, classList } = namespace2048

dragSensitivity = 50

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
    }

type alias Field = {
    array : Array Tile,
    height : Int,
    width : Int
}

empty : Int -> Int -> Field
empty w h = {
        array = Array.repeat (h*w) NoTile,
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

put : Tile -> Int -> Int -> Field -> Field
put tile x y field = { field | array = Array.set (xy2pos field x y) tile field.array }

drop : Int -> Int -> Int -> Field -> Maybe Field
drop val x y field =
    case (get x y field) of
        NoTile -> Just (put (Tile val Dropped) x y field)
        _ -> Nothing

hole : Field -> Bool
hole field = Array.foldr (\n hole -> hole || n==NoTile) False field.array

get : Int -> Int -> Field -> Tile
get x y field = 
  case Array.get (xy2pos field x y) field.array of
    Just v -> v
    Nothing -> NoTile

-- List (pos,tile) -> (score,List tile)
rshrink : List (Int,Tile) -> (Int, List Tile)
rshrink ns = let
        merge (pos,tile) (score,tiles) = case (List.head tiles) of
            Just next_tile -> case (tile,next_tile) of
                (Tile p _, Tile q _) -> if p == q
                    then (score+p+q, [NoTile, Tile (p+q) (Merged pos p)]++(List.drop 1 tiles))
                    else (score, [Tile p (Moved pos)]++tiles)
                (Tile p _, NoTile) -> (score, [Tile p (Moved pos)]++(List.drop 1 tiles))
                (NoTile, _) -> (score, tiles)
            Nothing -> (score, [case tile of
                    Tile n _ -> Tile n (Moved pos)
                    NoTile -> NoTile
                ])
    in
        List.foldr merge (0,[]) ns |> mapSecond (List.filter ((/=)NoTile))

hslice : Field -> Int -> List (Int,Tile)
hslice field y = List.map (\x -> (xy2pos field x y, get x y field)) (xs field) |> List.filter (\(_,tile) -> tile /= NoTile)

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

doShrink direct slice = case direct of
    Forward -> rshrink slice
    Backward -> rshrink (List.reverse slice) |> mapSecond List.reverse

doPad w direct (score,slice) = let
        zeros slice = List.repeat (w - (List.length slice)) NoTile
    in case direct of
        Forward -> (score, (zeros slice)++slice)
        Backward -> (score, slice++(zeros slice))

shift : Orientation -> Direction -> Field -> (Int, Field)
shift orient direct field = let
        tfield = doTranspose orient field
        (scores, slices) = List.foldl (\(score, slice) (scores, slices) -> (scores+score, slices++slice)) (0,[])
            <| List.map ((hslice tfield) >> (doShrink direct) >> (doPad tfield.width direct)) (ys tfield)
    in
        (scores, doTranspose orient { tfield | array = Array.fromList slices })


canShift : Orientation -> Direction -> Field -> Bool
canShift orient dir field = field /= second ( shift orient dir field )

cantShiftField : Field -> { up: Bool, down: Bool, left: Bool, right: Bool }
cantShiftField field = {
        up = not (canShift Vert Backward field),
        down = not (canShift Vert Forward field),
        left = not (canShift Hor Backward field),
        right = not (canShift Hor Forward field)
    }

toStringTile field tile = let
        toStringXY pos = (toString <| first <| pos2xy field pos)++" "++(toString <| second <| pos2xy field pos)
    in
        case tile of
            NoTile -> ""
            Tile n t -> (toString n) ++ " " ++ case t of
                Dropped -> "Drop"
                Moved pos -> "Moved from "++(toStringXY pos)
                Merged pos val -> "Merged from "++(toStringXY pos)++" "++(toString val)



render : Field -> Html msg
render field = let
        renderRow y = div [] (List.map (\x -> renderTile x y) (xs field))
        renderTile x y = let 
                tile = get x y field
            in
                div [class (tileClass tile)] [ text (toStringTile field tile) ]
    in
        pre [] (List.map (\y -> renderRow y) (ys field))

type alias ModelFields =
     { current: Field
     , before_drop: Field
     , before_move: Field
    }

type alias Model =
    { score : Int
    , drag : Maybe (Int,Int)
    , fields: ModelFields
    }


type Msg
    = Up
    | Down
    | Left
    | Right
    | Rand Int Int Int
    | Nop
    | DragStart Int Int
    | DragEnd Int Int

orient2msg : Orientation -> Direction -> Msg
orient2msg orient direct = case (orient,direct) of
    (Vert,Backward) -> Up
    (Vert,Forward) -> Down
    (Hor,Backward) -> Left
    (Hor,Forward) -> Right

rand field = 
    Random.generate 
        (\(is2,(x,y)) -> Rand (if is2 then 2 else 4) x y) 
        (Random.pair Random.bool (Random.pair (Random.int 0 (maxx field)) (Random.int 0 (maxy field))))

move : Orientation -> Direction -> Model -> (Model, Cmd Msg)
move orient dir model = let
        (score, newfield) = shift orient dir model.fields.current
    in if newfield == model.fields.current
        then (model, Cmd.none)
        else 
            ( { model 
              | score = score + model.score
              , fields = ModelFields newfield newfield model.fields.current
              }
            , rand newfield
            )
    
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Up -> move Vert Backward model
        Down -> move Vert Forward model
        Left -> move Hor Backward model
        Right -> move Hor Forward model
        Rand v x y -> 
            if hole model.fields.current
                then case (drop v x y model.fields.current) of
                    Just newfield -> ( { model 
                        | fields = ModelFields newfield model.fields.before_drop model.fields.before_move
                        }, Cmd.none)
                    Nothing -> (model, rand model.fields.current)
                else (model, Cmd.none)
        Nop -> (model, Cmd.none)
        DragStart sx sy -> (Model model.score (Just (sx,sy) ) model.fields, Cmd.none)
        DragEnd ex ey -> case model.drag of
            Nothing -> (model, Cmd.none)
            Just (sx,sy) -> let
                    dx = ex - sx
                    dy = ey - sy
                    orient = if (abs dx) > (abs dy) then Hor else Vert
                    dir = case orient of
                        Hor -> if dx > 0 then Forward else Backward
                        Vert -> if dy > 0 then Forward else Backward
                in
                    if (abs dx) + (abs dy) < dragSensitivity
                        then (Model model.score Nothing model.fields, Cmd.none)
                        else move orient dir model

view : Model -> Html Msg
view model =
    let 
        cant = cantShiftField model.fields.current
    in div []
        [ div [class [BoardTitle]] 
            [
                div [class [Score]] [text (toString model.score)]
            ]
        , div 
            [ class [Board]
            , onTouchStart (\pos -> DragStart (round pos.clientX) (round pos.clientY) )
            , onTouchEnd (\pos -> DragEnd (round pos.clientX) (round pos.clientY) )
            ]
            [ render model.fields.current
            , render model.fields.before_drop
            , render model.fields.before_move
    --        , button [onClick Up, disabled cant.up] [text "Up"]
    --        , button [onClick Down, disabled cant.down] [text "Down"]
    --        , button [onClick Left, disabled cant.left] [text "Left"]
    --        , button [onClick Right, disabled cant.right] [text "Right"]
            ]
        ]

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch 
    [ Keyboard.downs <| \code -> case code of
            37 -> Left
            38 -> Up
            39 -> Right
            40 -> Down
            _ -> Nop
    , if model.drag == Nothing 
        then Mouse.downs (\pos -> DragStart pos.x pos.y)
        else Mouse.ups (\pos -> DragEnd pos.x pos.y)
    ]   

init : (Model, Cmd Msg)
init = 
    let 
        e = empty boardWidth boardHeight
        model = Model 0 Nothing (ModelFields e e e)
    in ( model, rand model.fields.current)
