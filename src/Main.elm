module Main exposing (Cell, Model, Msg(..), init, main, update, view)

import Array exposing (Array)
import Array2D exposing (Array2D)
import Browser
import Browser.Events exposing (onAnimationFrame)
import Css exposing (..)
import Debug
import Html
import Html.Attributes exposing (src)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (onClick)
import Task
import Time


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


type alias Piece =
    ( Grid, ( Int, Int ) )


type PieceType
    = LongBoy
    | TriFry



---- MODEL ----


type alias Model =
    { board : Grid
    , activePiece : Maybe Piece
    , turnDuration : Int
    , stalledSinceTime : Time.Posix
    }


init : ( Model, Cmd Msg )
init =
    ( { board = initializeBoard
      , activePiece = Just (initializePiece TriFry)
      , turnDuration = 200
      , stalledSinceTime = Time.millisToPosix 0
      }
    , Cmd.none
    )


initializePiece : PieceType -> Piece
initializePiece pieceType =
    let
        pieceGrid =
            case pieceType of
                LongBoy ->
                    Array2D.fromList
                        [ [ Empty, Turquoise, Empty, Empty ]
                        , [ Empty, Turquoise, Empty, Empty ]
                        , [ Empty, Turquoise, Empty, Empty ]
                        , [ Empty, Turquoise, Empty, Empty ]
                        ]

                TriFry ->
                    Array2D.fromList
                        [ [ Empty, Purple, Empty, Empty ]
                        , [ Purple, Purple, Purple, Empty ]
                        , [ Empty, Empty, Empty, Empty ]
                        , [ Empty, Empty, Empty, Empty ]
                        ]
    in
    ( pieceGrid, ( 4, 0 ) )



---- UPDATE ----


type Msg
    = HandleTick Time.Posix
    | UpdateStallTime Time.Posix
    | NoOp


getCurrentTime : Cmd Msg
getCurrentTime =
    Task.perform UpdateStallTime Time.now


diffTime : Time.Posix -> Time.Posix -> Int
diffTime firstTime secondTime =
    Time.posixToMillis firstTime - Time.posixToMillis secondTime


tickActivePiece : Model -> Piece -> Time.Posix -> ( Piece, Time.Posix )
tickActivePiece { stalledSinceTime, turnDuration } ( grid, ( leftOffset, downOffset ) ) currentTime =
    let
        timeSince =
            diffTime currentTime stalledSinceTime
    in
    if timeSince > turnDuration then
        ( ( grid, ( leftOffset, downOffset + 1 ) ), currentTime )

    else
        ( ( grid, ( leftOffset, downOffset ) ), stalledSinceTime )


doNextTick : Model -> Time.Posix -> ( Model, Cmd Msg )
doNextTick model time =
    case model.activePiece of
        Just piece ->
            let
                ( updatedActivePiece, updatedStalledSinceTime ) =
                    tickActivePiece model piece time
            in
            ( { model | activePiece = Just updatedActivePiece, stalledSinceTime = updatedStalledSinceTime }, Cmd.none )

        Nothing ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleTick time ->
            doNextTick model time

        --         UpdateStallTime newStallTime ->
        --             ( { model | stalledSinceTime = newStallTime }, Cmd.none )
        _ ->
            ( model, Cmd.none )


applyPiece : Grid -> Maybe Piece -> Grid
applyPiece board maybePiece =
    case maybePiece of
        Just ( pieceGrid, ( leftOffset, downOffset ) ) ->
            Array2D.indexedFoldl
                (\columnIndex rowIndex cell accBoard ->
                    Array2D.set (columnIndex + downOffset) (rowIndex + leftOffset) cell accBoard
                )
                board
                pieceGrid

        Nothing ->
            board



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ css containerStyle ]
        [ renderBoard model
        ]


renderBoard : Model -> Html Msg
renderBoard { board, activePiece } =
    div [ css boardStyle ]
        (applyPiece board activePiece
            |> Array2D.indexedMap renderCell
            |> Array2D.rowMap (\row -> div [ css rowStyle ] (Array.toList row))
            |> Array.toList
        )


renderCell : Int -> Int -> Cell -> Html Msg
renderCell columnIndex rowIndex cell =
    div
        [ css (cellStyle cell)
        ]
        []


cellBaseStyle : List Style
cellBaseStyle =
    [ width (px 30)
    , height (px 30)
    ]


coloredCellBaseStyle : List Style
coloredCellBaseStyle =
    cellBaseStyle ++ [ border3 (px 1) solid (rgb 120 120 120) ]


cellStyle : Cell -> List Style
cellStyle cell =
    case cell of
        Turquoise ->
            coloredCellBaseStyle ++ [ backgroundColor (rgb 64 224 208) ]

        Purple ->
            coloredCellBaseStyle ++ [ backgroundColor (rgb 127 0 255) ]

        _ ->
            cellBaseStyle


boardStyle : List Style
boardStyle =
    [ displayFlex
    , flexDirection column
    , alignItems center
    , border3 (px 1) solid (rgb 120 120 120)
    , width (px 300)
    ]


containerStyle : List Style
containerStyle =
    [ displayFlex
    , alignItems center
    , flexDirection column
    ]


rowStyle : List Style
rowStyle =
    [ displayFlex
    ]



---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions model =
    onAnimationFrame HandleTick


main : Program () Model Msg
main =
    Browser.element
        { view = view >> toUnstyled
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
