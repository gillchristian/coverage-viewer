module Data exposing (..)

import Dict exposing (Dict)
import Json.Decode as D


type Entry
    = Entry String Float


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
        (D.field "date" D.string)
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
        {
          "date": "16/11/2020",
          "value": 21.54
        },
        {
          "date": "5/2/2021",
          "value": 23.84
        },
        {
          "date": "1/3/2021",
          "value": 23.99
        }
      ],
      "branches": [
        {
          "date": "16/11/2020",
          "value": 19.82
        },
        {
          "date": "5/2/2021",
          "value": 17.19
        },
        {
          "date": "1/3/2021",
          "value": 17.19
        }
      ],
      "functions": [
        {
          "date": "16/11/2020",
          "value": 19.64
        },
        {
          "date": "5/2/2021",
          "value": 16.96
        },
        {
          "date": "1/3/2021",
          "value": 16.91
        }
      ],
      "lines": [
        {
          "date": "16/11/2020",
          "value": 21.77
        },
        {
          "date": "5/2/2021",
          "value": 24.31
        },
        {
          "date": "1/3/2021",
          "value": 24.45
        }
      ]
    },
    "project-k": {
      "statements": [
        {
          "date": "16/11/2020",
          "value": 18.64
        },
        {
          "date": "5/2/2021",
          "value": 25.80
        },
        {
          "date": "1/3/2021",
          "value": 25.70
        }
      ],
      "branches": [
        {
          "date": "16/11/2020",
          "value": 26.53
        },
        {
          "date": "5/2/2021",
          "value": 27.13
        },
        {
          "date": "1/3/2021",
          "value": 26.72
        }
      ],
      "functions": [
        {
          "date": "16/11/2020",
          "value": 16.92
        },
        {
          "date": "5/2/2021",
          "value": 27.42
        },
        {
          "date": "1/3/2021",
          "value": 27.42
        }
      ],
      "lines": [
        {
          "date": "16/11/2020",
          "value": 17.82
        },
        {
          "date": "5/2/2021",
          "value": 26.43
        },
        {
          "date": "1/3/2021",
          "value": 26.32
        }
      ]
    },
    "project-c": {
      "statements": [
        {
          "date": "16/11/2020",
          "value": 0.00
        },
        {
          "date": "5/2/2021",
          "value": 0.00
        },
        {
          "date": "1/3/2021",
          "value": 0.00
        }
      ]
    },
    "project-d": {
      "branches": [
        {
          "date": "16/11/2020",
          "value": 0.00
        },
        {
          "date": "5/2/2021",
          "value": 0.00
        },
        {
          "date": "1/3/2021",
          "value": 0.00
        }
      ],
      "functions": [
        {
          "date": "16/11/2020",
          "value": 0.00
        },
        {
          "date": "5/2/2021",
          "value": 0.00
        },
        {
          "date": "1/3/2021",
          "value": 0.00
        }
      ],
      "lines": [
        {
          "date": "16/11/2020",
          "value": 0.00
        },
        {
          "date": "5/2/2021",
          "value": 0.00
        },
        {
          "date": "1/3/2021",
          "value": 0.00
        }
      ]
    },
    "project-e": {
      "statements": [
        {
          "date": "16/11/2020",
          "value": 63.20
        },
        {
          "date": "5/2/2021",
          "value": 77.62
        },
        {
          "date": "1/3/2021",
          "value": 79.33
        }
      ],
      "branches": [
        {
          "date": "16/11/2020",
          "value": 61.97
        },
        {
          "date": "5/2/2021",
          "value": 76.42
        },
        {
          "date": "1/3/2021",
          "value": 77.43
        }
      ],
      "functions": [
        {
          "date": "16/11/2020",
          "value": 67.96
        },
        {
          "date": "5/2/2021",
          "value": 80.28
        },
        {
          "date": "1/3/2021",
          "value": 82.48
        }
      ],
      "lines": [
        {
          "date": "16/11/2020",
          "value": 64.27
        },
        {
          "date": "5/2/2021",
          "value": 77.30
        },
        {
          "date": "1/3/2021",
          "value": 78.69
        }
      ]
    },
    "project-f": {
      "statements": [
        {
          "date": "16/11/2020",
          "value": 92.19
        },
        {
          "date": "5/2/2021",
          "value": 92.19
        },
        {
          "date": "1/3/2021",
          "value": 92.19
        }
      ],
      "branches": [
        {
          "date": "16/11/2020",
          "value": 58.33
        },
        {
          "date": "5/2/2021",
          "value": 58.33
        },
        {
          "date": "1/3/2021",
          "value": 58.33
        }
      ],
      "functions": [
        {
          "date": "16/11/2020",
          "value": 100.00
        },
        {
          "date": "5/2/2021",
          "value": 100.00
        },
        {
          "date": "1/3/2021",
          "value": 100.00
        }
      ],
      "lines": [
        {
          "date": "16/11/2020",
          "value": 92.06
        },
        {
          "date": "5/2/2021",
          "value": 92.06
        },
        {
          "date": "1/3/2021",
          "value": 92.06
        }
      ]
    },
    "project-g": {
      "statements": [
        {
          "date": "16/11/2020",
          "value": 31.02
        },
        {
          "date": "5/2/2021",
          "value": 32.99
        },
        {
          "date": "1/3/2021",
          "value": 32.94
        }
      ],
      "branches": [
        {
          "date": "16/11/2020",
          "value": 20.77
        },
        {
          "date": "5/2/2021",
          "value": 23.14
        },
        {
          "date": "1/3/2021",
          "value": 23.16
        }
      ],
      "functions": [
        {
          "date": "16/11/2020",
          "value": 22.15
        },
        {
          "date": "5/2/2021",
          "value": 25.62
        },
        {
          "date": "1/3/2021",
          "value": 25.76
        }
      ],
      "lines": [
        {
          "date": "16/11/2020",
          "value": 31.21
        },
        {
          "date": "5/2/2021",
          "value": 33.15
        },
        {
          "date": "1/3/2021",
          "value": 33.10
        }
      ]
    },
    "project-h": {
      "statements": [
        {
          "date": "16/11/2020",
          "value": 29.50
        },
        {
          "date": "5/2/2021",
          "value": 31.32
        },
        {
          "date": "1/3/2021",
          "value": 31.38
        }
      ],
      "branches": [
        {
          "date": "16/11/2020",
          "value": 21.55
        },
        {
          "date": "5/2/2021",
          "value": 18.90
        },
        {
          "date": "1/3/2021",
          "value": 18.90
        }
      ],
      "functions": [
        {
          "date": "16/11/2020",
          "value": 32.13
        },
        {
          "date": "5/2/2021",
          "value": 29.47
        },
        {
          "date": "1/3/2021",
          "value": 29.44
        }
      ],
      "lines": [
        {
          "date": "16/11/2020",
          "value": 29.28
        },
        {
          "date": "5/2/2021",
          "value": 30.87
        },
        {
          "date": "1/3/2021",
          "value": 30.93
        }
      ]
    },
    "project-i": {
      "statements": [
        {
          "date": "16/11/2020",
          "value": 2.52
        },
        {
          "date": "5/2/2021",
          "value": 2.52
        },
        {
          "date": "1/3/2021",
          "value": 2.52
        }
      ],
      "branches": [
        {
          "date": "16/11/2020",
          "value": 9.72
        },
        {
          "date": "5/2/2021",
          "value": 9.72
        },
        {
          "date": "1/3/2021",
          "value": 9.72
        }
      ],
      "functions": [
        {
          "date": "16/11/2020",
          "value": 4.26
        },
        {
          "date": "5/2/2021",
          "value": 4.26
        },
        {
          "date": "1/3/2021",
          "value": 4.26
        }
      ],
      "lines": [
        {
          "date": "16/11/2020",
          "value": 1.87
        },
        {
          "date": "5/2/2021",
          "value": 1.87
        },
        {
          "date": "1/3/2021",
          "value": 1.87
        }
      ]
    },
    "project-j": {
      "statements": [
        {
          "date": "16/11/2020",
          "value": 84.41
        },
        {
          "date": "5/2/2021",
          "value": 81.99
        },
        {
          "date": "1/3/2021",
          "value": 81.95
        }
      ],
      "branches": [
        {
          "date": "16/11/2020",
          "value": 68.81
        },
        {
          "date": "5/2/2021",
          "value": 64.80
        },
        {
          "date": "1/3/2021",
          "value": 67.65
        }
      ],
      "functions": [
        {
          "date": "16/11/2020",
          "value": 79.65
        },
        {
          "date": "5/2/2021",
          "value": 78.36
        },
        {
          "date": "1/3/2021",
          "value": 78.98
        }
      ],
      "lines": [
        {
          "date": "16/11/2020",
          "value": 83.77
        },
        {
          "date": "5/2/2021",
          "value": 82.51
        },
        {
          "date": "1/3/2021",
          "value": 82.09
        }
      ]
    }
  }
  """
