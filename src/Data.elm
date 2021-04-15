module Data exposing (..)

import Basics exposing (always)
import Basics.Extra exposing (uncurry)
import Dict as Dict exposing (Dict)
import Dict.Extra as Dict
import Iso8601 exposing (toTime)
import Json.Decode as D
import Json.Decode.Extra as D
import List as List
import List.Extra as List
import Maybe as Maybe
import Maybe.Extra as Maybe
import Result as Result exposing (toMaybe)
import Result.Extra as Result
import String as String
import Time exposing (Posix)
import Tuple exposing (first, pair)


type Section
    = Statements
    | Branches
    | Functions
    | Lines


type alias SectionValues =
    { values : List Entry
    , repo : String
    , section : Section
    }


type alias Entry =
    { date : Posix
    , value : Float
    }


type alias Repo =
    { statements : List Entry
    , branches : List Entry
    , functions : List Entry
    , lines : List Entry
    }


type alias Repos =
    Dict String Repo



-- JSON parsing


entry : D.Decoder Entry
entry =
    D.map2 Entry
        (D.field "date" D.datetime)
        (D.field "value" D.float)


repo : D.Decoder Repo
repo =
    D.map4 Repo
        (D.field "statements" <| D.list entry)
        (D.field "branches" <| D.list entry)
        (D.field "functions" <| D.list entry)
        (D.field "lines" <| D.list entry)


repos : D.Decoder Repos
repos =
    D.dict repo



-- CSV parsing


note : String -> Maybe a -> Result String a
note =
    Result.fromMaybe


lines : String -> List String
lines =
    String.trim
        >> String.split "\n"
        >> List.map String.trim


cells : String -> List String
cells =
    String.split ","


parseDate : String -> Result String Posix
parseDate date =
    case List.map String.toInt <| String.split "/" date of
        [ Just d, Just m, Just y ] ->
            let
                -- "25/12/2020" -> "2020-12-25T00:00:00Z"
                -- "5/12/2020" ->  "2020-12-05T00:00:00Z"
                isoDate =
                    padLeft y ++ "-" ++ padLeft m ++ "-" ++ padLeft d ++ "T00:00:00Z"
            in
            isoDate
                |> toTime
                |> Result.mapError (always <| "Failed to parse ISO date (" ++ isoDate ++ ") into Posix")

        _ ->
            Err <| "Invalid date: " ++ date


padLeft : Int -> String
padLeft d =
    if d > 9 then
        String.fromInt d

    else
        "0" ++ String.fromInt d


parsePercent : String -> Result String Float
parsePercent n =
    -- "50.6%" -> 50.6
    n
        |> String.replace "%" ""
        |> String.toFloat
        |> note ("Invalid float: " ++ n)


parseSectionName : String -> Result String Section
parseSectionName s =
    case s of
        "statements" ->
            Ok Statements

        "branches" ->
            Ok Branches

        "functions" ->
            Ok Functions

        "lines" ->
            Ok Lines

        _ ->
            Err <| "Unexpected section: " ++ s


parseSection : List Posix -> List String -> Result String SectionValues
parseSection dates values =
    -- ["project-a", "lines", "50"%, "60"%]
    case values of
        -- [r, s, ...vs]
        r :: s :: vs ->
            let
                parsedEntries =
                    Ok List.zip
                        |> Result.andMap (Ok dates)
                        -- map :      (String -> Result String Float) -> List String -> List (Result String Float)
                        -- traverse : (String -> Result String Float) -> List String -> Result String (List Float)
                        |> Result.andMap (Result.combineMap parsePercent vs)
                        |> Result.map (List.map (uncurry Entry))
            in
            -- SectionValues : List Entry ->        String ->       Section ->       SectionValues
            --          Maybe (List Entry) -> Maybe String -> Maybe Section -> Maybe SectionValues
            Ok SectionValues
                |> Result.andMap parsedEntries
                |> Result.andMap (Ok r)
                |> Result.andMap (parseSectionName s)

        _ ->
            Err <| "Invalid section: " ++ String.join "," values


addEmptyFirstCells : List (List String) -> List (List String)
addEmptyFirstCells rows =
    let
        f row ( acc, head ) =
            case row of
                h :: rest ->
                    if h == "" then
                        ( (head :: rest) :: acc, head )

                    else
                        ( (h :: rest) :: acc, h )

                l ->
                    ( l :: acc, head )
    in
    first <| List.foldl f ( [], "" ) rows


headingAndEntries : List String -> Result String ( List Posix, List String )
headingAndEntries rows =
    case rows of
        [] ->
            Err "No lines"

        [ _ ] ->
            Err "Only one line"

        heading :: rest ->
            Ok (\dates -> ( dates, rest ))
                |> Result.andMap (heading |> cells |> List.drop 2 |> Result.combineMap parseDate)


tupleSequenceResult : ( a, Result c b ) -> Result c ( a, b )
tupleSequenceResult ( a, b ) =
    Result.map (pair a) b


groupByRepos : ( List Posix, List String ) -> Result String Repos
groupByRepos ( dates, rows ) =
    rows
        |> List.map cells
        |> addEmptyFirstCells
        |> Result.combineMap (parseSection dates)
        |> Result.map (Dict.groupBy (\v -> v.repo))
        |> Result.map (Dict.map (\k v -> repoFromSections v))
        |> Result.map Dict.toList
        |> Result.andThen (Result.combineMap tupleSequenceResult)
        |> Result.map Dict.fromList


repoFromSections : List SectionValues -> Result String Repo
repoFromSections sections =
    Ok Repo
        |> Result.andMap (Result.map .values <| note "Missing statements" <| List.find (\s -> s.section == Statements) sections)
        |> Result.andMap (Result.map .values <| note "Missing branches" <| List.find (\s -> s.section == Branches) sections)
        |> Result.andMap (Result.map .values <| note "Missing functions" <| List.find (\s -> s.section == Functions) sections)
        |> Result.andMap (Result.map .values <| note "Missing lines" <| List.find (\s -> s.section == Lines) sections)


parseCsv : String -> Result String Repos
parseCsv =
    lines >> headingAndEntries >> Result.andThen groupByRepos


sampleDataCsv : String
sampleDataCsv =
    """
Repository,Sections,16/11/2020,5/2/2021,1/3/2021
project-a,statements,21.54%,23.84%,23.99%
,branches,19.82%,17.19%,17.19%
,functions,19.64%,16.96%,16.91%
,lines,21.77%,24.31%,24.45%
project-b,statements,18.64%,25.80%,25.70%
,branches,26.53%,27.13%,26.72%
,functions,16.92%,27.42%,27.42%
,lines,17.82%,26.43%,26.32%
project-c,statements,0.00%,0.00%,0.00%
,branches,0.00%,0.00%,0.00%
,functions,0.00%,0.00%,0.00%
,lines,0.00%,0.00%,0.00%
project-d,statements,63.20%,77.62%,79.33%
,branches,61.97%,76.42%,77.43%
,functions,67.96%,80.28%,82.48%
,lines,64.27%,77.30%,78.69%
project-e,statements,92.19%,92.19%,92.19%
,branches,58.33%,58.33%,58.33%
,functions,100.00%,100.00%,100.00%
,lines,92.06%,92.06%,92.06%
project-f,statements,31.02%,32.99%,32.94%
,branches,20.77%,23.14%,23.16%
,functions,22.15%,25.62%,25.76%
,lines,31.21%,33.15%,33.10%
project-g,statements,29.50%,31.32%,31.38%
,branches,21.55%,18.90%,18.90%
,functions,32.13%,29.47%,29.44%
,lines,29.28%,30.87%,30.93%
project-h,statements,2.52%,2.52%,2.52%
,branches,9.72%,9.72%,9.72%
,functions,4.26%,4.26%,4.26%
,lines,1.87%,1.87%,1.87%
project-i,statements,84.41%,81.99%,81.95%
,branches,68.81%,64.80%,67.65%
,functions,79.65%,78.36%,78.98%
,lines,83.77%,82.51%,82.09%
  """
