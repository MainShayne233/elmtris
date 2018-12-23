module Main exposing (Cell, Model, Msg(..), init, main, update, view)

import Array exposing (Array)
import Array2D exposing (Array2D)
import Browser
import Css exposing (..)
import Html
import Html.Attributes exposing (src)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (onClick)


type Cell
    = Red
    | Green
    | Blue
    | Orange
    | Purple
    | Yellow
    | Turquoise
    | Empty


type alias Grid =
    Array2D Cell


initializeBoard =
    Array2D.initialize 20 10 (\_ _ -> Empty)



---- MODEL ----


type alias Model =
    { board : Grid
    }


init : ( Model, Cmd Msg )
init =
    ( { board = initializeBoard
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ renderBoard model.board
        ]


renderBoard : Grid -> Html Msg
renderBoard board =
    div [ css boardStyle ]
        (Array2D.indexedMap renderCell board
            |> Array2D.rowMap (\row -> div [ css rowStyle ] (Array.toList row))
            |> Array.toList
        )


renderCell : Int -> Int -> Cell -> Html Msg
renderCell rowIndex columnIndex cell =
    div
        [ css (cellStyle cell)
        ]
        []


cellStyle : Cell -> List Style
cellStyle cell =
    [ width (px 30)
    , height (px 30)
    , border3 (px 1) solid (rgb 120 120 120)
    ]


boardStyle : List Style
boardStyle =
    [ displayFlex
    , flexDirection column
    , alignItems center
    ]


rowStyle : List Style
rowStyle =
    [ displayFlex
    ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view >> toUnstyled
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
