module Rain exposing
  ( Model, init
  , Msg, update
  , subscriptions
  , view
  )

{-| This module contains the core functions of the app.

# Model
@docs Model, init

# Update
@docs Msg, update

# Subscriptions
@docs subscriptions

# View
@docs view

-}

import Html
import Html.App as Html
import List
import List.Extra as List
import Maybe
import Raindrop
import Random
import Svg
import Svg.Attributes as Svg
import Task
import Time
import Type
import Window


{-| Application state. -}
type alias Model =
  { raindrops : List Raindrop.Model
  , windowWidth : Int
  , windowHeight : Int
  , randomSeed : Random.Seed
  }


{-| Messages that update the state. -}
type Msg
  = Tick Time.Time
  | WindowSizeErr String
  | WindowSizeOk Window.Size
  | FromRaindrop Raindrop.Msg


{-| Initialize the application with a default `Model`. -}
init : (Model, Cmd Msg)
init =
  ( Model [] 1280 720 <| Random.initialSeed 0
  , getWindowSize
  )


{-| Return a random pair of coordinates within the window. It takes the window
size as the Type.Coord`.
-}
randomCoord : Type.Coord -> Random.Seed -> (Type.Coord, Random.Seed)
randomCoord (windowWidth, windowHeight) seed =
  let -- The size of the biggest raindrop is 80x80. We use 90x90 for a little
      -- more padding when the random coordinates lie on a window edge.
      maxWidth = windowWidth - 90
      maxHeight = windowHeight - 90
      generator = Random.pair (Random.int 90 maxWidth) (Random.int 90 maxHeight)
  in Random.step generator seed


{-| Initialize the raindrops in the `Model`. -}
initRaindrops : Type.Coord -> (List Raindrop.Model, Random.Seed)
initRaindrops winCoord =
  let seed = Random.initialSeed 0
      (stages, seed') = Random.step (Random.list 10 (Random.int 0 5)) seed
      f stage (raindrops, seed'') =
        let (coord, newSeed) = randomCoord winCoord seed''
        in (Raindrop.init coord (toStage stage) :: raindrops, newSeed)
  in List.foldr f ([], seed') stages


{-| Return the `Raindrop.LifeCycle` stage that the given `Int` maps to.

At present, `0` maps to `Raindrop.Stage1`, `1` maps to Raindrop.Stage2`, and so
on. It might conceivably be changed to weight raindrop stages differently. For
example, if it's given a percentage (`0` - `99`), it could assign
`Raindrop.Stage3` to the interval `[30,70]`, which means we'll see more of this
kind of raindrop, and less of the other types.
-}
toStage : Int -> Raindrop.LifeCycle
toStage n =
  let stages =
        [ Raindrop.Stage1
        , Raindrop.Stage2
        , Raindrop.Stage3
        , Raindrop.Stage4
        , Raindrop.Stage5
        , Raindrop.Stage6
        ]
  in Maybe.withDefault Raindrop.Stage1 <| List.getAt n stages


{-| Update the application state. -}
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      let (raindrops', seed) =
            updateRaindrops (model.windowWidth, model.windowHeight)
              model.raindrops model.randomSeed
      in ( { model |
               raindrops = raindrops',
               randomSeed = seed
           }
         , Cmd.none
         )

    WindowSizeErr _ ->
      (model, Cmd.none)

    WindowSizeOk windowSize ->
      let newModel =
            { model |
                windowWidth = windowSize.width,
                windowHeight = windowSize.height,
                randomSeed = seed,
                raindrops = raindrops'
            }
          (raindrops', seed) = initRaindrops (windowSize.width, windowSize.height)
      in (newModel, Cmd.none)

    FromRaindrop _ ->
      (model, Cmd.none)


{-| Update all the raindrops. It takes the window size as the Type.Coord`. -}
updateRaindrops : Type.Coord -> List Raindrop.Model -> Random.Seed -> (List Raindrop.Model, Random.Seed)
updateRaindrops coord raindrops seed =
  let f model (raindrops', seed') =
        let (model', seed'') = updateRaindrop coord model seed'
        in (model' :: raindrops', seed'')
  in List.foldr f ([], seed) raindrops


{-| Update a `Raindrop.Model`. It takes the window size as the Type.Coord`. -}
updateRaindrop : Type.Coord -> Raindrop.Model -> Random.Seed -> (Raindrop.Model, Random.Seed)
updateRaindrop coord model seed =
  if Raindrop.isEol model then
    let (coord', seed') = randomCoord coord seed
    in (Raindrop.new coord' Raindrop.Stage1, seed')

  else
    (Raindrop.grow model, seed)


{-| Return subscriptions to event sources. -}
subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every (Time.millisecond * 120) Tick


{-| View application state as HTML. -}
view : Model -> Html.Html Msg
view model =
  Svg.svg
    [ Svg.viewBox
        <| "0 0 "
             ++ toString model.windowWidth
             ++ " "
             ++ toString model.windowHeight
    ]
    (List.map viewRaindrop model.raindrops)


{-| Transform `Raindrop` messages to our own `Msg`. -}
viewRaindrop : Raindrop.Model -> Html.Html Msg
viewRaindrop model =
  Html.map FromRaindrop <| Raindrop.view model


{-| Return the window size. -}
getWindowSize : Cmd Msg
getWindowSize =
  Task.perform WindowSizeErr WindowSizeOk Window.size
