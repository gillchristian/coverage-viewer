module Main exposing (..)

import Array as Array exposing (Array)
import Browser
import Color exposing (Color)
import Color.Manipulate as Manipulate
import Data exposing (..)
import Dict as Dict exposing (Dict)
import File exposing (File)
import Html
import Html.Attributes as Attr
import Html.Events exposing (on, onClick, onInput)
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
import Maybe as Maybe
import Platform.Sub as Sub
import Random as Random
import Result as Result
import Result.Extra as Result
import String
import Svg
import Svg.Attributes as SvgAttr
import Task
import Time exposing (Month(..))



---- MODEL ----


type Category
    = Statements
    | Branches
    | Functions
    | Lines


type alias ReposWithColor =
    Dict String ( Color, Repo )


type alias Model =
    { dataInput : String
    , data : Maybe (Result String ReposWithColor)
    , hinted : Maybe Entry
    , category : Category
    }



-- INIT


init : String -> ( Model, Cmd Msg )
init _ =
    ( { dataInput = ""
      , data = Nothing
      , hinted = Nothing
      , category = Statements
      }
    , Cmd.none
    )



-- API


setData : Maybe (Result String ReposWithColor) -> Model -> Model
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
    = ChangeCategory Category
    | Hint (Maybe Entry)
    | OnInputChange String
    | SubmitInput
    | LoadSampleData
    | ParseDataAndSetOffset String Int
    | GotFile (Maybe File)
    | FileLoaded String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                |> addCmd (Random.generate (ParseDataAndSetOffset model.dataInput) genColorOffset)

        LoadSampleData ->
            model
                |> addCmd (Random.generate (ParseDataAndSetOffset Data.sampleDataCsv) genColorOffset)

        ParseDataAndSetOffset data offset ->
            model
                |> setData (Just <| decodeData offset data)
                |> addCmd Cmd.none

        GotFile (Just file) ->
            ( model, Task.perform FileLoaded <| File.toString file )

        GotFile Nothing ->
            ( model, Cmd.none )

        FileLoaded fileData ->
            model
                |> addCmd (Random.generate (ParseDataAndSetOffset fileData) genColorOffset)


addCmd : Cmd Msg -> Model -> ( Model, Cmd Msg )
addCmd cmd model =
    ( model, cmd )


genColorOffset : Random.Generator Int
genColorOffset =
    Random.int 0 <| Array.length availableColors



-- VIEW


mkLine : Category -> ( String, ( Color, Repo ) ) -> LineChart.Series Entry
mkLine category ( name, ( color, repo ) ) =
    let
        line =
            LineChart.line color Dots.circle name
    in
    case category of
        Statements ->
            line repo.statements

        Branches ->
            line repo.branches

        Functions ->
            line repo.functions

        Lines ->
            line repo.lines


view : Model -> Html.Html Msg
view model =
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
                [ textarea "JSON or CSV Coverage Data" "{ ... } " model.dataInput OnInputChange ]
            , Html.input
                [ Attr.type_ "file"
                , Attr.multiple False
                , on "change" (D.map GotFile filesDecoder)
                ]
                []
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


fileInput : Html.Html Msg
fileInput =
    Html.div
        [ Attr.class "mt-1 sm:mt-0 sm:col-span-2"
        ]
        [ Html.div
            [ Attr.class "max-w-lg flex justify-center px-6 pt-5 pb-6 border-2 border-gray-300 border-dashed rounded-md"
            ]
            [ Html.div
                [ Attr.class "space-y-1 text-center"
                ]
                [ Svg.svg
                    [ SvgAttr.class "mx-auto h-12 w-12 text-gray-400"
                    , SvgAttr.stroke "currentColor"
                    , SvgAttr.fill "none"
                    , SvgAttr.viewBox "0 0 48 48"
                    , Attr.attribute "aria-hidden" "true"
                    ]
                    [ Svg.path
                        [ SvgAttr.d "M28 8H12a4 4 0 00-4 4v20m32-12v8m0 0v8a4 4 0 01-4 4H12a4 4 0 01-4-4v-4m32-4l-3.172-3.172a4 4 0 00-5.656 0L28 28M8 32l9.172-9.172a4 4 0 015.656 0L28 28m0 0l4 4m4-24h8m-4-4v8m-12 4h.02"
                        , SvgAttr.strokeWidth "2"
                        , SvgAttr.strokeLinecap "round"
                        , SvgAttr.strokeLinejoin "round"
                        ]
                        []
                    ]
                , Html.div
                    [ Attr.class "flex text-sm text-gray-600"
                    ]
                    [ Html.label
                        [ Attr.for "file-upload"
                        , Attr.class "relative cursor-pointer bg-white rounded-md font-medium text-indigo-600 hover:text-indigo-500 focus-within:outline-none focus-within:ring-2 focus-within:ring-offset-2 focus-within:ring-indigo-500"
                        ]
                        [ Html.span []
                            [ Html.text "Upload a file" ]
                        , Html.input
                            [ Attr.type_ "file"

                            -- TODO - Adding sr-only breaks the action
                            --      - Removing it breaks the styles
                            -- , Attr.class "sr-only"
                            , Attr.multiple False
                            , Attr.id "file-upload"
                            , Attr.name "file-upload"
                            , on "change" (D.map GotFile filesDecoder)
                            ]
                            []
                        ]
                    , Html.p
                        [ Attr.class "pl-1"
                        ]
                        [ Html.text "or drag and drop" ]
                    ]
                , Html.p
                    [ Attr.class "text-xs text-gray-500"
                    ]
                    [ Html.text "PNG, JPG, GIF up to 10MB" ]
                ]
            ]
        ]


filesDecoder : D.Decoder (Maybe File)
filesDecoder =
    D.map List.head <| D.at [ "target", "files" ] (D.list File.decoder)



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
            [ ( "Date"
              , \{ date } -> formatDate date
              )
            , ( categoryToStr model.category
              , \{ value } -> String.fromFloat value ++ "%"
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
        , range = Range.padded 20 20
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


decodeJson : Int -> String -> Result String ReposWithColor
decodeJson offset =
    D.decodeString Data.repos
        >> Result.mapError D.errorToString
        >> Result.map (addColorsToRepos offset)


decodeCsv : Int -> String -> Result String ReposWithColor
decodeCsv offset =
    parseCsv
        >> Result.map (addColorsToRepos offset)


decodeData : Int -> String -> Result String ReposWithColor
decodeData offset data =
    decodeCsv offset data
        |> Result.orElseLazy (\_ -> decodeJson offset data)


addColorsToRepos : Int -> Repos -> ReposWithColor
addColorsToRepos offset =
    Dict.toList
        >> Array.fromList
        >> Array.indexedMap (colorAndRepo offset)
        >> Array.toList
        >> Dict.fromList


colorAndRepo : Int -> Int -> ( String, Repo ) -> ( String, ( Color, Repo ) )
colorAndRepo offset i ( key, repo ) =
    let
        colorCount =
            Array.length availableColors

        color =
            Array.get (modBy colorCount <| i + offset) availableColors
                |> Maybe.withDefault Colors.cyan
    in
    ( key, ( color, repo ) )


availableColors : Array Color
availableColors =
    Array.fromList
        [ Colors.pink
        , Colors.blue
        , Colors.gold
        , Colors.red
        , Colors.green
        , Colors.cyan
        , Colors.teal
        , Colors.purple
        , Colors.rust
        , Colors.gray
        ]



-- PROGRAM


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
