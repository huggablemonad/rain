module Raindrop exposing
  ( Model, init
  , Msg, update
  , view
  , LifeCycle(..), new, grow, isEol
  )

{-| This module provides the different raindrop shapes and functions to
initialize and update them.

# Model
@docs Model, init

# Update
@docs Msg, update

# View
@docs view

# API
@docs LifeCycle, new, grow, isEol

-}

import String
import Svg
import Svg.Attributes as Svg
import Type


{-| Initialize the raindrop with the given `Type.Coord` and life cycle stage. -}
init : Type.Coord -> LifeCycle -> Model
init coord lifeCycle =
  Model coord lifeCycle


{-| Raindrop state. -}
type alias Model =
  { coord : Type.Coord
  , lifeCycle : LifeCycle
  }


{-| Raindrop life cycle.

There're five visible stages. The sixth is when it disappears.
-}
type LifeCycle
  = Stage1
  | Stage2
  | Stage3
  | Stage4
  | Stage5
  | Stage6  -- Raindrop disappears.


{-| Messages that update the raindrop state. -}
type Msg
  = New Type.Coord LifeCycle
  | Grow


{-| Update the raindrop state. -}
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    New coord lifeCycle ->
      ( Model coord lifeCycle
      , Cmd.none
      )

    Grow ->
      ( { model | lifeCycle = nextStage model.lifeCycle }
      , Cmd.none
      )


{-| Return a new raindrop with the given `Type.Coord` and life cycle stage. -}
new : Type.Coord -> LifeCycle -> Model
new coord lifeCycle =
  Model coord lifeCycle


{-| Update the raindrop to the next stage of its life cycle. -}
grow : Model -> Model
grow model =
  { model | lifeCycle = nextStage model.lifeCycle }


{-| Return `True` if the raindrop is at the end of its life cycle. -}
isEol : Model -> Bool
isEol model =
  model.lifeCycle == Stage6


{-| View the raindrop as an `SVG`. -}
view : Model -> Svg.Svg Msg
view model =
  raindrop model.coord model.lifeCycle


{-| Return the `SVG` that corresponds to the raindrop's life cycle stage. -}
raindrop : Type.Coord -> LifeCycle -> Svg.Svg Msg
raindrop coord lifeCycle =
  case lifeCycle of
    Stage1 ->
      raindrop1 coord

    Stage2 ->
      raindrop2 coord

    Stage3 ->
      raindrop3 coord

    Stage4 ->
      raindrop4 coord

    Stage5 ->
      raindrop5 coord

    Stage6 ->
      raindrop6 coord


{-| Return the next stage of the raindrop's life cycle. -}
nextStage : LifeCycle -> LifeCycle
nextStage lifeCycle =
  case lifeCycle of
    Stage1 ->
      Stage2

    Stage2 ->
      Stage3

    Stage3 ->
      Stage4

    Stage4 ->
      Stage5

    Stage5 ->
      Stage6

    Stage6 ->
      Stage1


{-| `SVG` for stage 1 of the raindrop life cycle.

    .
-}
raindrop1 : Type.Coord -> Svg.Svg Msg
raindrop1 (x, y) =
  Svg.g
    [ Svg.stroke "white"
    , Svg.strokeWidth "1"
    ]
    [ Svg.circle
        [ Svg.cx <| toString x
        , Svg.cy <| toString y
        , Svg.r "2"
        , Svg.fill "transparent"
        ]
        []
    ]


{-| `SVG` for stage 2 of the raindrop life cycle.

    o
-}
raindrop2 : Type.Coord -> Svg.Svg Msg
raindrop2 (x, y) =
  Svg.g
    [ Svg.stroke "white"
    , Svg.strokeWidth "1"
    ]
    [ Svg.circle
        [ Svg.cx <| toString x
        , Svg.cy <| toString y
        , Svg.r "5"
        , Svg.fill "transparent"
        ]
        []
    ]


{-| `SVG` for stage 3 of the raindrop life cycle.

    O
-}
raindrop3 : Type.Coord -> Svg.Svg Msg
raindrop3 (x, y) =
  Svg.g
    [ Svg.stroke "white"
    , Svg.strokeWidth "1"
    ]
    [ Svg.circle
        [ Svg.cx <| toString x
        , Svg.cy <| toString y
        , Svg.r "8"
        , Svg.fill "transparent"
        ]
        []
    ]


{-| `SVG` for stage 4 of the raindrop life cycle.

     -
    |.|
     -
-}
raindrop4 : Type.Coord -> Svg.Svg Msg
raindrop4 (x, y) =
  Svg.g
    [ Svg.stroke "white"
    , Svg.strokeWidth "1"
    ]
    [ Svg.rect
        [ Svg.x << toString <| x - 15
        , Svg.y << toString <| y - 15
        , Svg.width <| toString 30
        , Svg.height <| toString 30
        , Svg.fill "transparent"
        ]
        []
    , raindrop1 (x, y)
    ]


{-| `SVG` for stage 5 of the raindrop life cycle.

      -
     / \
    | O |
     \ /
      -
-}
raindrop5 : Type.Coord -> Svg.Svg Msg
raindrop5 (x, y) =
  Svg.g
    [ Svg.stroke "white"
    , Svg.strokeWidth "1"
    ]
    [ Svg.polyline
        [ Svg.points
            <| let x1 = x - 15
                   y1 = y - 20
                   x2 = x + 15
                   y2 = y1
                   x3 = x2 + 15
                   y3 = y2 + 10
                   x4 = x3
                   y4 = y3 + 20
                   x5 = x2
                   y5 = y4 + 10
                   x6 = x1
                   y6 = y5
                   x7 = x6 - 15
                   y7 = y5 - 10
                   x8 = x7
                   y8 = y7 - 20
                   x9 = x8 + 15
                   y9 = y8 - 10
               in String.join " "
                    <| List.map toString [ x1, y1, x2, y2, x3, y3, x4, y4
                                         , x5, y5, x6, y6, x7, y7, x8, y8
                                         , x9, y9
                                         ]
        , Svg.fill "transparent"
        ]
        []
    , raindrop3 (x, y)
    ]


{-| Transparent rectangle the size of `raindrop5`. -}
raindrop6 : Type.Coord -> Svg.Svg Msg
raindrop6 (x, y) =
  Svg.g
    []
    [ Svg.rect
        [ Svg.x << toString <| x - 20
        , Svg.y << toString <| y - 20
        , Svg.width "40"
        , Svg.height "40"
        , Svg.fill "transparent"
        ]
        []
    ]
