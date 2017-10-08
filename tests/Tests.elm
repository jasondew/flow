module Tests exposing (..)

import Test exposing (..)
import Expect
import Main exposing (..)


all : Test
all =
    describe "patternToCells"
        [ test "parses a single border cell" <|
            \() ->
                Expect.equal [ borderAt 0 0 ] (Main.patternToCells "#")
        , test "ignores preceding and trailing newlines" <|
            \() ->
                Expect.equal
                    [ borderAt 0 0
                    , blankAt 0 1
                    , pipeAt 0 2
                    , blankAt 0 3
                    , borderAt 0 4
                    ]
                    (Main.patternToCells "\n# = #\n")
        , test "parses multiple lines" <|
            \() ->
                Expect.equal
                    [ borderAt 0 0
                    , borderAt 0 1
                    , borderAt 0 2
                    , borderAt 1 0
                    , blankAt 1 1
                    , borderAt 1 2
                    , borderAt 2 0
                    , pipeAt 2 1
                    , borderAt 2 2
                    ]
                    (Main.patternToCells "###\n# #\n#=#\n")
        ]


borderAt : Int -> Int -> Cell
borderAt row column =
    Cell (Location row column) Border


pipeAt : Int -> Int -> Cell
pipeAt row column =
    Cell (Location row column) (Pipe Empty)


blankAt : Int -> Int -> Cell
blankAt row column =
    Cell (Location row column) Blank
