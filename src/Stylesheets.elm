port module Stylesheets exposing (..)

import Css.File exposing (CssCompilerProgram, CssFileStructure)
import Styles

port files : CssFileStructure -> Cmd msg

fileStructure : CssFileStructure
fileStructure =
    Css.File.toFileStructure
        [ ( "2048.css", Css.File.compile [ Styles.css ] ) ]


main : CssCompilerProgram
main =
    Css.File.compiler files fileStructure
