module Routing exposing (..)

import Navigation
import UrlParser exposing ((</>), map, oneOf, s, top)


type Route
    = Home
    | About
    | NotFound


routes : { user : String, about : String }
routes =
    { user = "user"
    , about = "about"
    }


parseLocation : Navigation.Location -> Route
parseLocation location =
    case UrlParser.parsePath matchers location of
        Just route ->
            route

        Nothing ->
            NotFound


matchers : UrlParser.Parser (Route -> a) a
matchers =
    oneOf
        [ map Home top
        , map About (s routes.about)
        ]


for : Route -> String
for route =
    let
        r list =
            "/" ++ String.join "/" list
    in
    case route of
        About ->
            r [ routes.about ]

        Home ->
            r [ "" ]

        NotFound ->
            r [ "" ]
