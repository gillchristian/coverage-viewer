module Main exposing (..)

import Browser
import Color.Manipulate as Manipulate
import Data exposing (..)
import Dict as Dict
import Html
import Json.Decode as D
import LineChart
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Intersection as Intersection
import LineChart.Axis.Line as AxisLine
import LineChart.Axis.Range as Range
import LineChart.Axis.Tick as Tick
import LineChart.Axis.Ticks as Ticks
import LineChart.Axis.Title as Title
import LineChart.Colors as Colors
import LineChart.Container as Container
import LineChart.Coordinate as Coordinate
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk
import LineChart.Legends as Legends
import LineChart.Line as Line
import List as List
import Random
import Random.Pipeline
import String
import Svg
import Time exposing (Month(..))



---- MODEL ----


type Category
    = Statements
    | Branches
    | Functions
    | Lines


type alias Model =
    { data : Result String Repos
    , hinted : Maybe Entry
    , category : Category
    }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( { data = Result.mapError D.errorToString <| D.decodeString Data.repos Data.data
      , hinted = Nothing
      , category = Statements
      }
    , Cmd.none
    )



-- API


setData : Result String Repos -> Model -> Model
setData repos model =
    { model | data = repos }


setHint : Maybe Entry -> Model -> Model
setHint hinted model =
    { model | hinted = hinted }


setCategory : Category -> Model -> Model
setCategory category model =
    { model | category = category }



-- UPDATE


type Msg
    = RecieveData (Result String Repos)
    | ChangeCategory Category
    | Hint (Maybe Entry)


update : Msg -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
update msg ( model, _ ) =
    case msg of
        RecieveData data ->
            model
                |> setData data
                |> addCmd Cmd.none

        ChangeCategory category ->
            model
                |> setHint Nothing
                |> setCategory category
                |> addCmd Cmd.none

        Hint point ->
            model
                |> setHint point
                |> addCmd Cmd.none


addCmd : Cmd Msg -> Model -> ( Model, Cmd Msg )
addCmd cmd model =
    ( model, Cmd.none )



-- VIEW


mkLine : Category -> ( String, Repo ) -> LineChart.Series Entry
mkLine category ( name, repo ) =
    -- TODO: line color
    case category of
        Statements ->
            LineChart.line (Manipulate.lighten 0.2 Colors.cyan) Dots.circle name repo.statements

        Branches ->
            LineChart.line (Manipulate.lighten 0.2 Colors.cyan) Dots.circle name repo.branches

        Functions ->
            LineChart.line (Manipulate.lighten 0.2 Colors.cyan) Dots.circle name repo.functions

        Lines ->
            LineChart.line (Manipulate.lighten 0.2 Colors.cyan) Dots.circle name repo.lines


view : ( Model, Cmd Msg ) -> Html.Html Msg
view ( model, _ ) =
    Html.div []
        [ case model.data of
            Ok data ->
                LineChart.viewCustom (chartConfig model) <|
                    List.map (mkLine model.category) <|
                        Dict.toList data

            Err error ->
                Html.div
                    []
                    [ Html.div [] [ Html.text "Failed to parse data, you messed up something with it" ]
                    , Html.div [] [ Html.text error ]
                    ]
        ]



-- CHART CONFIG


chartConfig : Model -> LineChart.Config Entry Msg
chartConfig model =
    { y = yAxisConfig
    , x = xAxisConfig
    , container = containerConfig
    , interpolation = Interpolation.monotone
    , intersection = Intersection.default
    , legends = Legends.default
    , events = eventsConfig
    , junk = Junk.default
    , grid = Grid.default
    , area = Area.default
    , line = lineConfig model.hinted
    , dots = Dots.custom (Dots.disconnected 4 2)
    }



-- CHART CONFIG / AXES


yAxisConfig : Axis.Config Entry Msg
yAxisConfig =
    Axis.custom
        { title = Title.default ""
        , variable = Just << .value
        , pixels = 450
        , range = Range.window 0.0 100.0
        , axisLine = AxisLine.rangeFrame Colors.gray
        , ticks =
            Ticks.custom <|
                \dataRange axisRange ->
                    [ tickRain ( 0.0, "0%" )
                    , tickRain ( 25.0, "25%" )
                    , tickRain ( 50.0, "50%" )
                    , tickRain ( 75.0, "75%" )
                    , tickRain ( 100.0, "100%" )
                    ]
        }


xAxisConfig : Axis.Config Entry Msg
xAxisConfig =
    Axis.custom
        { title = Title.default "Time"
        , variable = Just << toFloat << Time.posixToMillis << .date
        , pixels = 1270
        , range = Range.default
        , axisLine = AxisLine.none
        , ticks = Ticks.default
        }



-- CHART CONFIG / AXES / TICKS


tickRain : ( Float, String ) -> Tick.Config msg
tickRain ( value, label ) =
    Tick.custom
        { position = value
        , color = Colors.gray
        , width = 1
        , length = 5
        , grid = True
        , direction = Tick.negative
        , label = Just (tickLabel label)
        }



-- tickString : String -> Tick.Config msg
-- tickString str =
--     Tick.custom
--         { position = str
--         , color = Colors.gray
--         , width = 1
--         , length = 5
--         , grid = False
--         , direction = Tick.negative
--         , label = Just (tickLabel str)
--         }


tickLabel : String -> Svg.Svg msg
tickLabel =
    Junk.label Colors.black



-- CHART CONFIG / CONTIANER


containerConfig : Container.Config Msg
containerConfig =
    Container.custom
        { attributesHtml = []
        , attributesSvg = []
        , size = Container.relative
        , margin = Container.Margin 30 180 30 70
        , id = "line-chart-lines"
        }



-- CHART CONFIG / EVENTS


eventsConfig : Events.Config Entry Msg
eventsConfig =
    Events.custom
        [ Events.onMouseMove Hint Events.getNearest
        , Events.onMouseLeave (Hint Nothing)
        ]



-- CHART CONFIG / LINE


lineConfig : Maybe Entry -> Line.Config Entry
lineConfig maybeHovered =
    Line.custom (toLineStyle maybeHovered)


toLineStyle : Maybe Entry -> List Entry -> Line.Style
toLineStyle maybeHovered lineData =
    case maybeHovered of
        Nothing ->
            Line.style 1 identity

        Just hovered ->
            if List.any ((==) hovered) lineData then
                Line.style 2 identity

            else
                Line.style 1 Manipulate.grayscale



-- UTILS


formatDate : Time.Posix -> String
formatDate t =
    (String.fromInt <| Time.toYear Time.utc t)
        ++ "/"
        ++ (monthToStr <| Time.toMonth Time.utc t)
        ++ "/"
        ++ (dayToStr <| Time.toDay Time.utc t)


dayToStr : Int -> String
dayToStr d =
    if d > 9 then
        String.fromInt d

    else
        "0" ++ String.fromInt d


monthToStr : Month -> String
monthToStr month =
    case month of
        Jan ->
            "01"

        Feb ->
            "02"

        Mar ->
            "03"

        Apr ->
            "04"

        May ->
            "05"

        Jun ->
            "06"

        Jul ->
            "07"

        Aug ->
            "08"

        Sep ->
            "09"

        Oct ->
            "10"

        Nov ->
            "11"

        Dec ->
            "12"



-- PROGRAM


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
