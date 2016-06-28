module Main exposing (main)

{-| A port of [rain](https://sources.debian.net/src/bsdgames/2.17-24/rain/)
from the [bsdgames](https://tracker.debian.org/pkg/bsdgames) package. It
"attempts to create [an animated] raindrop effect"
(<https://sources.debian.net/src/bsdgames/2.17-24/README/>).

<https://huggablemonad.github.io/rain/>

@docs main

-}

import Html.App as Html
import Rain


{-| Main entry point. -}
main : Program Never
main =
  Html.program
    { init = Rain.init
    , update = Rain.update
    , subscriptions = Rain.subscriptions
    , view = Rain.view
    }
