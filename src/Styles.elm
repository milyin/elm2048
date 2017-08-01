module Styles exposing (..)

import Css exposing (..)
import Css.Elements exposing (..)
import Css.Colors exposing (..)
import Css.Namespace exposing (namespace)
import Html.CssHelpers

namespace2048 =
    Html.CssHelpers.withNamespace ""

type CssClasses = Board | BoardRow | BoardTitle | Score | BoardTile | VisibleTile | TileText 
    | T0 | T2 | T4 | T8 | T16 | T32 | T64 | T128 | T256 | T512 | T1024 | T2048 | TBig | THuge

visibleTileClass n = [ VisibleTile, 
    case n of 
        0 -> T0
        2 -> T2
        4 -> T4
        8 -> T8
        16 -> T16
        32 -> T32
        64 -> T64
        128 -> T128
        256 -> T256
        512 -> T512
        1024 -> T1024
        2048 -> T2048
        _  -> if n < 9999 then TBig else THuge
    ]

css = let
        boardSize = 98.0
        tileSize = boardSize / 4 -- use it for font size only: all other sizes are relative to parents.
                                -- Font size in CSS can't be relative to parent so we apply transform to it 
                                -- when field is not 4x4
        fontSizeNorm = tileSize / 2
        fontSizeTBig = tileSize / 3
        fontSizeTHuge = tileSize / 4
        setFont parent fontsz = 
            class parent 
                [ children 
                    [ class TileText
                        [
                            fontSize (vmin fontsz)
                        ]
                    ]
                ]
    in (stylesheet << namespace namespace2048.name)
    [
        div [
            padding (px 0),
            margin (px 0)
        ],
        class Board
        [
            backgroundColor gray,
            borderRadius (vw 1),
            width (vmin boardSize),
            height (vmin boardSize),
            displayFlex,
            flexDirection column,
            alignItems stretch,
            justifyContent spaceAround
        ],
        class BoardRow
        [
            displayFlex,
            flexDirection row,
            alignItems stretch,
            justifyContent spaceAround,
            flexBasis (pct 100)
        ],
        class BoardTile
        [
            displayFlex,
            flexDirection row,
            alignItems stretch,
            justifyContent spaceAround,
            flexBasis (pct 100)
        ],
        class VisibleTile
        [
            display inlineFlex,
            flexBasis (pct 100),
            alignItems center,
            justifyContent center,
            borderRadius (vmin 1),
            color (rgb 255 255 255),
            property "user-select" "none",
            margin (vmin 0.25)
        ],
        class TileText [
            justifyContent center,
            alignItems center,
            fontSize (vmin fontSizeNorm)
        ],
        class T0 [],
        class T2 [ backgroundColor (rgb 238 228 218) ],
        class T4 [ backgroundColor (rgb 237 224 200) ],
        class T8 [ backgroundColor (rgb 242 177 121) ],
        class T16 [ backgroundColor (rgb 242 177 121) ],
        class T32 [ backgroundColor (rgb 246 124 95) ],

        class T32 [ backgroundColor (rgb 246 124 95) ],
        class T64 [ backgroundColor (rgb 246 124 95) ],
        class T128 [ backgroundColor (rgb 237 207 114) ],
        class T256 [ backgroundColor (rgb 237 204 97) ],
        class T512 [ backgroundColor (rgb 237 200 80) ],
        class T1024 [ backgroundColor (rgb 237 197 63) ],
        class T2048 [ backgroundColor (rgb 237 194 46) ],
        class TBig [ backgroundColor (rgb 60 58 50) ],
        class THuge [ backgroundColor (rgb 60 58 50) ],
        setFont T1024 fontSizeTBig,
        setFont T2048 fontSizeTBig,
        setFont TBig fontSizeTBig,
        setFont THuge fontSizeTHuge
    ]

