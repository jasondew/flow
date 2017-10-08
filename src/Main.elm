port module Main exposing (..)

import AnimationFrame
import Html exposing (..)
import Html.Attributes exposing (..)
import Time exposing (Time)
import Mouse
import List.Extra


port requestOffsets : () -> Cmd msg


port receiveOffsets : (( Offset, Offset ) -> msg) -> Sub msg



---- MODEL ----


type alias Model =
    { cells : List Cell
    , boardOffset : Offset
    , cellOffset : Offset
    }


type alias Cell =
    { location : Location
    , content : CellContent
    }


type CellContent
    = Border
    | Pipe PipeState


type PipeState
    = Empty
    | Filling Time
    | Filled


type alias Location =
    { row : Int
    , column : Int
    }


type alias Offset =
    { x : Int, y : Int }


animationTime : Time
animationTime =
    1000


init : ( Model, Cmd Msg )
init =
    ( { cells = initialCells
      , boardOffset = Offset 0 0
      , cellOffset = Offset 0 0
      }
    , requestOffsets ()
    )


initialCells : List Cell
initialCells =
    patternToCells """
##########
#        #
#        #
#        #
#=       #
#        #
#        #
#        #
#        #
##########
    """


patternToCells : String -> List Cell
patternToCells pattern =
    pattern
        |> String.lines
        |> List.filter (\line -> String.length line > 0)
        |> List.indexedMap (\row line -> lineToCells row line)
        |> List.concat


lineToCells : Int -> String -> List Cell
lineToCells row string =
    string
        |> String.toList
        |> List.indexedMap (\index char -> ( index, char ))
        |> List.filterMap (charToCell row)


charToCell : Int -> ( Int, Char ) -> Maybe Cell
charToCell row ( column, char ) =
    let
        location =
            Location row column

        content =
            case char of
                '#' ->
                    Just Border

                ' ' ->
                    Nothing

                '=' ->
                    Just (Pipe <| Filling animationTime)

                _ ->
                    Nothing
    in
        Maybe.map (Cell location) content



---- UPDATE ----


type Msg
    = Tick Time
    | Click Mouse.Position
    | ReceiveOffset ( Offset, Offset )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updatedModel =
            case msg of
                Tick timeElapsed ->
                    step timeElapsed model

                Click position ->
                    handleClick position model

                ReceiveOffset ( boardOffset, cellOffset ) ->
                    { model
                        | boardOffset = boardOffset
                        , cellOffset = cellOffset
                    }
    in
        ( updatedModel, Cmd.none )


step : Time -> Model -> Model
step timeElapsed model =
    { model | cells = List.map (stepCell timeElapsed) model.cells }


stepCell : Time -> Cell -> Cell
stepCell timeElapsed cell =
    case cell.content of
        Pipe state ->
            case state of
                Filling timeLeft ->
                    if timeLeft <= 0 then
                        { cell | content = Pipe Filled }
                    else
                        { cell | content = Pipe (Filling <| timeLeft - timeElapsed) }

                _ ->
                    cell

        _ ->
            cell


handleClick : Mouse.Position -> Model -> Model
handleClick position model =
    let
        location =
            locationFromPosition model position

        newCell =
            Cell location (Pipe <| Filling animationTime)
    in
        case findCell model.cells location of
            Just cell ->
                model

            Nothing ->
                { model | cells = newCell :: model.cells }


findCell : List Cell -> Location -> Maybe Cell
findCell cells location =
    List.Extra.find (\cell -> cell.location == location) cells


locationFromPosition : Model -> Mouse.Position -> Location
locationFromPosition model position =
    let
        row =
            (position.y - model.boardOffset.y) // model.cellOffset.y

        column =
            (position.x - model.boardOffset.x) // model.cellOffset.x
    in
        Location row column



---- VIEW ----


view : Model -> Html Msg
view model =
    div
        []
        [ cellsView model
        , statusView model
        ]


cellsView : Model -> Html Msg
cellsView model =
    model.cells
        |> List.map cellView
        |> div [ id "board" ]


cellView : Cell -> Html Msg
cellView cell =
    div (cellAttributes cell) []


cellAttributes : Cell -> List (Attribute Msg)
cellAttributes { location, content } =
    let
        contentAttributes =
            case content of
                Border ->
                    [ class "border" ]

                Pipe Empty ->
                    [ class "pipe" ]

                Pipe Filled ->
                    [ class "filled pipe" ]

                Pipe (Filling timeLeft) ->
                    let
                        percentFilled =
                            ((animationTime - timeLeft) / animationTime) * 100
                    in
                        [ class "filling pipe"
                        , style
                            [ ( "background"
                              , "linear-gradient(90deg, #1E5799 0%, #1E5799 "
                                    ++ (toString percentFilled)
                                    ++ "%, white "
                                    ++ (toString <| percentFilled + 90)
                                    ++ "%)"
                              )
                            ]
                        ]

        rowClass =
            "row-" ++ (toString location.row)

        columnClass =
            "col-" ++ (toString location.column)
    in
        List.append
            contentAttributes
            [ [ "cell", rowClass, columnClass ] |> String.join " " |> class ]


statusView : Model -> Html Msg
statusView model =
    text ""



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Mouse.clicks Click
        , receiveOffsets ReceiveOffset
        ]
