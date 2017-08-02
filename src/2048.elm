module Elm2048 exposing (Model, Msg, update, view, subscriptions, init)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Array exposing (..)
import Random
import Styles exposing (..)
import Keyboard exposing (..)
import Mouse exposing (Position)
import Json.Decode as Decode
import TouchEvents exposing (onTouchStart, onTouchEnd, Touch)
import Tuple exposing (..)
import Time exposing (Time, millisecond, inMilliseconds)
import Css exposing (asPairs, transform, translate2, pct, vmin, width, height)

{ id, class, classList } = namespace2048

dragSensitivity = 50
moveAnimationTime = 100*millisecond
timerInterval = 5*millisecond
boardWidth = 4
boardHeight = 4

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
    }

type TileType = Dropped | Moved Int | Merged (Int,Int) (Int,Int)

type Tile = Tile Int TileType | NoTile

type alias Field = {
    array : Array Tile,
    height : Int,
    width : Int
}

type AnimateState = AnimateStop | AnimateMoveStart | AnimateMove Time Time

type alias Model =
    { score : Int
    , drag : Maybe (Int,Int)
    , field: Field
    , animateState: AnimateState
    }

toAt : Time -> Time -> Time -> Float
toAt interval start cur = (inMilliseconds (cur - start)) / interval

type Msg
    = Up
    | Down
    | Left
    | Right
    | Rand Int (Int,Int)
    | Nop
    | DragStart Int Int
    | DragEnd Int Int
    | Tick Time


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

xy2pos : Field -> (Int, Int) -> Int
xy2pos field (x,y) = y*field.width + x

put : Tile -> (Int, Int) -> Field -> Field
put tile xy field = { field | array = Array.set (xy2pos field xy) tile field.array }

drop : Int -> (Int, Int) -> Field -> Maybe Field
drop val xy field =
    case (get xy field) of
        NoTile -> Just (put (Tile val Dropped) xy field)
        _ -> Nothing

hole : Field -> Bool
hole field = Array.foldr (\n hole -> hole || n==NoTile) False field.array

get : (Int, Int) -> Field -> Tile
get xy field = 
  case Array.get (xy2pos field xy) field.array of
    Just v -> v
    Nothing -> NoTile

-- List (pos,tile) -> (score,List tile)
rshrink : List (Int,Tile) -> (Int, List Tile)
rshrink ns = let
        merge (apos,atile) (score,tiles) = case (List.head tiles) of
            Just (bpos, btile) -> case (atile,btile) of
                (Tile a _, Tile b _) -> if a == b
                    then (score+a+b, [(apos,NoTile), (apos, Tile (a+b) (Merged (apos,a) (bpos,b))) ]++(List.drop 1 tiles))
                    else (score, [(apos, Tile a (Moved apos))]++tiles)
                (Tile a _, NoTile) -> (score, [(apos, Tile a (Moved apos))]++(List.drop 1 tiles))
                (NoTile, _) -> (score, tiles)
            Nothing -> (score, [(apos, case atile of
                    Tile a _ -> Tile a (Moved apos)
                    NoTile -> NoTile
                )])
    in
        List.foldr merge (0,[]) ns |> mapSecond (List.map second >> List.filter ((/=)NoTile))

hslice : Field -> Int -> List (Int,Tile)
hslice field y = List.map (\x -> (xy2pos field (x,y), get (x,y) field)) (xs field) |> List.filter (\(_,tile) -> tile /= NoTile)

transposeTile : Field -> Field -> Tile -> Tile
transposeTile srcfield dstfield tile = let
        swapxy (x,y) = (y,x)
    in case tile of
        NoTile -> NoTile
        Tile n t -> Tile n <| case t of
            Moved pos -> Moved (xy2pos dstfield <| swapxy <| pos2xy srcfield pos)
            Merged (apos,a) (bpos,b) -> Merged 
                ((xy2pos dstfield <| swapxy <| pos2xy srcfield apos),a)
                ((xy2pos dstfield <| swapxy <| pos2xy srcfield bpos),b)
            Dropped -> Dropped

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
            get (y,x) field |> transposeTile field newfield
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

orient2msg : Orientation -> Direction -> Msg
orient2msg orient direct = case (orient,direct) of
    (Vert,Backward) -> Up
    (Vert,Forward) -> Down
    (Hor,Backward) -> Left
    (Hor,Forward) -> Right

rand field = 
    Random.generate 
        (\(is2, xy) -> Rand (if is2 then 2 else 4) xy) 
        (Random.pair Random.bool (Random.pair (Random.int 0 (maxx field)) (Random.int 0 (maxy field))))

move : Orientation -> Direction -> Model -> (Model, Cmd Msg)
move orient dir model = let
        (score, newfield) = shift orient dir model.field
    in if newfield == model.field
        then (model, Cmd.none)
        else 
            ( { model 
              | score = score + model.score
              , field = newfield
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
        Rand v xy -> 
            if hole model.field
                then case (drop v xy model.field) of
                    Just newfield -> ( { model 
                        | field = newfield, animateState = AnimateMoveStart
                        }, Cmd.none)
                    Nothing -> (model, rand model.field)
                else (model, Cmd.none) -- later start animate drop here
        Nop -> (model, Cmd.none)
        DragStart sx sy -> ( { model | drag = Just (sx,sy) }, Cmd.none)
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
                        then ( { model | drag = Nothing}, Cmd.none)
                        else move orient dir model
        Tick time -> case model.animateState of
            AnimateMoveStart -> ( { model | animateState = AnimateMove time time }, Cmd.none)
            AnimateMove start _ -> ( 
                { model | animateState = 
                    if (toAt moveAnimationTime start time) > 1
                        then AnimateStop
                        else AnimateMove start time 
                }, Cmd.none )
            AnimateStop -> (model, Cmd.none)

styled = asPairs >> style

type AnimateAt = Stop | Move Float | Drop Float

animateAt : Model -> AnimateAt
animateAt model = case model.animateState of
    AnimateMoveStart -> Move 0
    AnimateMove start cur -> Move (toAt moveAnimationTime start cur)
    _ -> Stop

renderTile transforms visual_classes display_text = 
    div [ (class [AnimableTile]), styled transforms ]
        [
            div [ class visual_classes ]
            [ 
                div [ class [TileText] ] 
                [ 
                    text display_text
                ] 
            ]
        ]

renderFinalTile animate tile = case (animate,tile) of
    (Stop, Tile n _) -> renderTile [] (visibleTileClass n) (toString n)
    (Move _, _) -> renderTile [] [Hidden] ""
    (Drop _, _) -> renderTile [] [Hidden] ""
    (_, NoTile) -> renderTile [] [Hidden] ""

moveTransform at (fromx,fromy) (tox,toy) = let
        toPct at from to = (toFloat (from-to))*(1.0-at)*100
    in [ transform <| translate2 (toPct at fromx tox  |> pct) (toPct at fromy toy |> pct) ]

renderSourceTile1 animate tile field xy = case (animate,tile) of
    (Stop, _) -> renderTile [] [Hidden] ""
    (Move at, Tile n Dropped) -> renderTile [] [Hidden] ""
    (Move at, Tile n (Moved pos)) -> renderTile 
        (moveTransform at (pos2xy field pos) xy)
        (visibleTileClass n)
        (toString n)
    (Move at, Tile n (Merged (pa,a) (_,_))) -> renderTile 
        (moveTransform at (pos2xy field pa) xy)
        (visibleTileClass a)
        (toString a)
    (Drop _, _) -> renderTile [] [Hidden] ""
    (_, NoTile) -> renderTile [] [Hidden] ""

renderSourceTile2 animate tile field xy = case (animate,tile) of
    (Stop, _) -> renderTile [] [Hidden] ""
    (Move at, Tile n Dropped) -> renderTile [] [Hidden] ""
    (Move at, Tile n (Moved _)) -> renderTile [] [Hidden] ""
    (Move at, Tile n (Merged (_,_) (pb,b))) -> renderTile 
        (moveTransform at (pos2xy field pb) xy)
        (visibleTileClass b)
        (toString b)
    (Drop _, _) -> renderTile [] [Hidden] ""
    (_, NoTile) -> renderTile [] [Hidden] ""


render : AnimateAt -> Field -> Html msg
render animate field = let
        renderBoardRow y = div [ class [BoardRow] ] (List.map (\x -> renderBoardTile animate (x,y)) (xs field))
        renderBoardTile animate xy = let 
                tile = get xy field
            -- BoardTile have no margins. So transform n*100% correctly moves it to n tiles
            in  div [ class [BoardTile] ]
                [ renderFinalTile animate tile
                , renderSourceTile1 animate tile field xy
                , renderSourceTile2 animate tile field xy
                ]
    in
        div [ class [Board] ] (List.map (\y -> renderBoardRow y) (ys field))


view : Model -> Html Msg
view model =
    let 
        cant = cantShiftField model.field
    in div []
        -- [ div [class [BoardTitle]] 
          --  [
          --      div [class [Score]] 
          --      [ text (toString model.score)
          --      , text " "
          --      , text (toString model.animate)
          --      ]
          --  ]
        [ div 
            [ onTouchStart (\pos -> DragStart (round pos.clientX) (round pos.clientY) )
            , onTouchEnd (\pos -> DragEnd (round pos.clientX) (round pos.clientY) )
            ]
            [ render (animateAt model) model.field
    --        , button [onClick Up, disabled cant.up] [text "Up"]
    --        , button [onClick Down, disabled cant.down] [text "Down"]
    --        , button [onClick Left, disabled cant.left] [text "Left"]
    --        , button [onClick Right, disabled cant.right] [text "Right"]
            ]
        ]

subscriptions : Model -> Sub Msg
subscriptions model = Sub.batch <|
    [ Keyboard.downs <| \code -> case code of
            37 -> Left
            38 -> Up
            39 -> Right
            40 -> Down
            _ -> Nop
    , if model.drag == Nothing 
        then Mouse.downs (\pos -> DragStart pos.x pos.y)
        else Mouse.ups (\pos -> DragEnd pos.x pos.y)
    , Time.every timerInterval Tick
    ]

init : (Model, Cmd Msg)
init = 
    let 
        e = empty boardWidth boardHeight
        model = Model 0 Nothing e AnimateStop
    in ( model, rand model.field )
