module Main exposing (..)

import Browser
import Color.Manipulate as Manipulate
import Data exposing (..)
import Dict as Dict
import Html
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
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
    { dataInput : String
    , data : Maybe (Result String Repos)
    , hinted : Maybe Entry
    , category : Category
    }



-- INIT


init : ( Model, Cmd Msg )
init =
    ( { dataInput = ""
      , data = Nothing
      , hinted = Nothing
      , category = Statements
      }
    , Cmd.none
    )



-- API


setData : Maybe (Result String Repos) -> Model -> Model
setData repos model =
    { model | data = repos }


setHint : Maybe Entry -> Model -> Model
setHint hinted model =
    { model | hinted = hinted }


setCategory : Category -> Model -> Model
setCategory category model =
    { model | category = category }


setDataInput : String -> Model -> Model
setDataInput str model =
    { model | dataInput = str }



-- UPDATE


type Msg
    = RecieveData (Maybe (Result String Repos))
    | ChangeCategory Category
    | Hint (Maybe Entry)
    | OnInputChange String
    | SubmitInput
    | LoadSampleData


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

        OnInputChange str ->
            model
                |> setDataInput str
                |> addCmd Cmd.none

        SubmitInput ->
            model
                |> setData (Just <| Result.mapError D.errorToString <| D.decodeString Data.repos model.dataInput)
                |> addCmd Cmd.none

        LoadSampleData ->
            model
                |> setData (Just <| Result.mapError D.errorToString <| D.decodeString Data.repos Data.sampleData)
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
    Html.div [ Attr.class "container mx-auto" ]
        [ Html.div
            [ Attr.classList
                [ ( "flex", True )
                , ( "items-end", True )
                , ( "p-4", True )
                ]
            ]
            [ Html.div
                [ Attr.class "mr-4" ]
                [ textarea "JSON Coverage Data" "{ ... } " model.dataInput OnInputChange ]
            , Html.div [] [ button "See coverage" SubmitInput ]
            , Html.div [ Attr.class "ml-4" ] [ button "Load sample data" LoadSampleData ]
            ]
        , case model.data of
            Nothing ->
                Html.text ""

            Just (Ok data) ->
                Html.div
                    []
                    [ categorySwith model.category
                    , Dict.toList data
                        |> List.map (mkLine model.category)
                        |> LineChart.viewCustom (chartConfig model)
                    ]

            Just (Err error) ->
                alert "Something is wrong with the data you provided" [ error ]
        ]


categorySwith : Category -> Html.Html Msg
categorySwith selected =
    Html.div
        [ Attr.class "flex space-x-4" ]
        [ radioGroupButton "Statements" (selected == Statements) (ChangeCategory Statements)
        , radioGroupButton "Branches" (selected == Branches) (ChangeCategory Branches)
        , radioGroupButton "Functions" (selected == Functions) (ChangeCategory Functions)
        , radioGroupButton "Lines" (selected == Lines) (ChangeCategory Lines)
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
    , junk =
        Junk.hoverOne model.hinted
            [ ( categoryToStr model.category
              , \datum -> String.fromFloat datum.value ++ "%"
              )
            ]
    , grid = Grid.default
    , area = Area.default
    , line = lineConfig model.hinted
    , dots = Dots.hoverOne model.hinted
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
                    [ tickPercentage ( 0.0, "0%" )
                    , tickPercentage ( 25.0, "25%" )
                    , tickPercentage ( 50.0, "50%" )
                    , tickPercentage ( 75.0, "75%" )
                    , tickPercentage ( 100.0, "100%" )
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
        , ticks = Ticks.intCustom 5 tickDate
        }



-- CHART CONFIG / AXES / TICKS


tickPercentage : ( Float, String ) -> Tick.Config msg
tickPercentage ( value, label ) =
    Tick.custom
        { position = value
        , color = Colors.gray
        , width = 1
        , length = 5
        , grid = True
        , direction = Tick.negative
        , label = Just (tickLabel label)
        }


tickDate : Int -> Tick.Config msg
tickDate i =
    let
        time =
            Time.millisToPosix i

        label =
            formatDate time
    in
    Tick.custom
        { position = toFloat <| Time.posixToMillis time
        , color = Colors.gray
        , width = 1
        , length = 5
        , grid = False
        , direction = Tick.negative
        , label = Just (tickLabel label)
        }


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
    -- Events.custom
    --     [ Events.onMouseMove Hint Events.getNearest
    --     , Events.onMouseLeave (Hint Nothing)
    --     ]
    Events.hoverOne Hint



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



-- UI Elements


radioGroupButton : String -> Bool -> Msg -> Html.Html Msg
radioGroupButton text active msg =
    Html.button
        [ onClick msg
        , Attr.classList
            [ ( "inline-flex", True )
            , ( "items-center", True )
            , ( "px-2.5", True )
            , ( "py-1.5", True )
            , ( "border-2", True )
            , ( "border-transparent", not active )
            , ( "border-indigo-700", active )
            , ( "text-xs", True )
            , ( "font-medium", True )
            , ( "rounded", True )
            , ( "shadow-sm", True )
            , ( "text-indigo-700", True )
            , ( "hover:text-white", True )
            , ( "hover:bg-indigo-700", True )
            , ( "focus:outline-none", True )
            , ( "focus:ring-2", True )
            , ( "focus:ring-offset-2", True )
            , ( "focus:ring-indigo-500", True )
            ]
        ]
        [ Html.text text ]


button : String -> Msg -> Html.Html Msg
button text msg =
    Html.button
        [ onClick msg
        , Attr.classList
            [ ( "inline-flex", True )
            , ( "items-center", True )
            , ( "px-2.5", True )
            , ( "py-1.5", True )
            , ( "border", True )
            , ( "border-transparent", True )
            , ( "text-xs", True )
            , ( "font-medium", True )
            , ( "rounded", True )
            , ( "", True )
            , ( "shadow-sm", True )
            , ( "text-white", True )
            , ( "bg-indigo-600", True )
            , ( "hover:bg-indigo-700", True )
            , ( "focus:outline-none", True )
            , ( "focus:ring-2", True )
            , ( "focus:ring-offset-2", True )
            , ( "focus:ring-indigo-500", True )
            ]
        ]
        [ Html.text text ]


textarea : String -> String -> String -> (String -> Msg) -> Html.Html Msg
textarea label placeholder value msg =
    Html.div
        []
        [ Html.label
            [ Attr.classList
                [ ( "block", True )
                , ( "text-sm", True )
                , ( "font-medium", True )
                , ( "text-gray-700", True )
                ]
            ]
            [ Html.text label ]
        , Html.div
            [ Attr.classList [ ( "mt-1", True ) ] ]
            [ Html.textarea
                [ Attr.value value
                , onInput msg
                , Attr.classList
                    [ ( "shadow-sm", True )
                    , ( "focus:ring-indigo-500", True )
                    , ( "focus:border-indigo-500", True )
                    , ( "block", True )
                    , ( "w-full", True )
                    , ( "sm:text-sm", True )
                    , ( "border-gray-300", True )
                    , ( "rounded-md", True )
                    ]
                , Attr.placeholder placeholder
                ]
                []
            ]
        ]


alert : String -> List String -> Html.Html Msg
alert title content =
    Html.div [ Attr.class "rounded-md bg-red-50 p-4" ]
        [ Html.div [ Attr.class "flex" ]
            [ Html.div [ Attr.class "ml-3" ]
                [ Html.h3 [ Attr.class "text-sm font-medium text-red-800" ]
                    [ Html.text title ]
                , Html.div [ Attr.class "mt-2 text-sm text-red-700" ]
                    [ content
                        |> List.map (\line -> Html.li [] [ Html.text line ])
                        |> Html.ul [ Attr.class "list-disc pl-5 space-y-1" ]
                    ]
                ]
            ]
        ]



-- UTILS


categoryToStr : Category -> String
categoryToStr cat =
    case cat of
        Statements ->
            "statements"

        Branches ->
            "branches"

        Functions ->
            "functions"

        Lines ->
            "lines"


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
