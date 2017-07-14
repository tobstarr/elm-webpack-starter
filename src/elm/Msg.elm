module Msg exposing (..)

import Navigation


type Msg
    = ChangeLocation String
    | OnLocationChange Navigation.Location
    | Increment
