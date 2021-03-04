module Data exposing (..)

import Dict exposing (Dict)
import Json.Decode as D
import Json.Decode.Extra as D
import Time exposing (Posix)


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


data : String
data =
    """
  {
    "project-a": {
      "statements": [
        { "date": "2020-06-16T00:00:00Z", "value": 21.54 },
        { "date": "2020-07-16T00:00:00Z", "value": 25.54 },
        { "date": "2020-08-16T00:00:00Z", "value": 30.54 },
        { "date": "2020-09-16T00:00:00Z", "value": 32.54 },
        { "date": "2020-10-16T00:00:00Z", "value": 35.54 },
        { "date": "2020-11-16T00:00:00Z", "value": 41.54 },
        { "date": "2020-12-16T00:00:00Z", "value": 48.54 },
        { "date": "2021-01-16T00:00:00Z", "value": 59.54 },
        { "date": "2021-02-16T00:00:00Z", "value": 70.54 },
        { "date": "2021-03-16T00:00:00Z", "value": 72.54 }
      ],
      "branches": [
        { "date": "2020-09-16T00:00:00Z", "value": 32.54 },
        { "date": "2020-10-16T00:00:00Z", "value": 35.54 },
        { "date": "2020-11-16T00:00:00Z", "value": 41.54 },
        { "date": "2020-12-16T00:00:00Z", "value": 48.54 },
        { "date": "2021-01-16T00:00:00Z", "value": 59.54 },
        { "date": "2021-02-16T00:00:00Z", "value": 70.54 },
        { "date": "2021-03-16T00:00:00Z", "value": 72.54 }
      ],
      "functions": [],
      "lines": []
    },
    "project-k": {
      "statements": [
        { "date": "2020-06-16T00:00:00Z", "value": 11.54 },
        { "date": "2020-07-16T00:00:00Z", "value": 15.54 },
        { "date": "2020-08-16T00:00:00Z", "value": 20.54 },
        { "date": "2020-09-16T00:00:00Z", "value": 32.54 },
        { "date": "2020-10-16T00:00:00Z", "value": 45.54 },
        { "date": "2020-11-16T00:00:00Z", "value": 41.54 },
        { "date": "2020-12-16T00:00:00Z", "value": 38.54 },
        { "date": "2021-01-16T00:00:00Z", "value": 49.54 },
        { "date": "2021-02-16T00:00:00Z", "value": 50.54 },
        { "date": "2021-03-16T00:00:00Z", "value": 52.54 }
      ],
      "branches": [
        { "date": "2020-09-16T00:00:00Z", "value": 32.54 },
        { "date": "2020-10-16T00:00:00Z", "value": 45.54 },
        { "date": "2020-11-16T00:00:00Z", "value": 41.54 },
        { "date": "2020-12-16T00:00:00Z", "value": 38.54 },
        { "date": "2021-01-16T00:00:00Z", "value": 49.54 },
        { "date": "2021-02-16T00:00:00Z", "value": 50.54 },
        { "date": "2021-03-16T00:00:00Z", "value": 52.54 }
      ],
      "functions": [],
      "lines": []
    },
    "project-e": {
      "statements": [
        { "date": "2020-06-16T00:00:00Z", "value": 81.54 },
        { "date": "2020-07-16T00:00:00Z", "value": 85.54 },
        { "date": "2020-08-16T00:00:00Z", "value": 70.54 },
        { "date": "2020-09-16T00:00:00Z", "value": 72.54 },
        { "date": "2020-10-16T00:00:00Z", "value": 65.54 },
        { "date": "2020-11-16T00:00:00Z", "value": 51.54 },
        { "date": "2020-12-16T00:00:00Z", "value": 38.54 },
        { "date": "2021-01-16T00:00:00Z", "value": 29.54 },
        { "date": "2021-02-16T00:00:00Z", "value": 20.54 },
        { "date": "2021-03-16T00:00:00Z", "value": 22.54 }
      ],
      "branches": [
        { "date": "2020-09-16T00:00:00Z", "value": 72.54 },
        { "date": "2020-10-16T00:00:00Z", "value": 65.54 },
        { "date": "2020-11-16T00:00:00Z", "value": 51.54 },
        { "date": "2020-12-16T00:00:00Z", "value": 38.54 },
        { "date": "2021-01-16T00:00:00Z", "value": 29.54 },
        { "date": "2021-02-16T00:00:00Z", "value": 20.54 },
        { "date": "2021-03-16T00:00:00Z", "value": 22.54 }
      ],
      "functions": [],
      "lines": []
    }
  }
  """
