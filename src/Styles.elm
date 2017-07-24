module Styles exposing (..)

import Css exposing (..)
import Css.Colors exposing (..)
import Css.Namespace exposing (namespace)
import Html.CssHelpers

namespace2048 =
    Html.CssHelpers.withNamespace "elm2048"

type CssClasses = Tile | T0 | T2 | T4 | T8 | T16 | T32 | 
    T64 | T128 | T256 | T512 | T1024 | T2048 | TBig

tileClass n = [ Tile, 
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
        _  -> TBig
    ]

css = (stylesheet << namespace namespace2048.name)
    [
        class Tile
        [
            display inlineBlock,
            borderRadius (px 8),
            color (rgb 255 255 255),
            margin (px 5),
            width (px 100),
            height (px 100),
            textAlign center,
            verticalAlign middle,
            lineHeight (px 100),
            fontSize (px 60)
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
        class TBig [ backgroundColor (rgb 60 58 50) ]
    ]