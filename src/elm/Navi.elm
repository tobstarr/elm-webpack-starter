module Navi exposing (..)

import Helpers exposing (linkTo)
import Html exposing (Html, a, button, div, li, span, text, ul)
import Html.Attributes exposing (attribute, class, href, id, type_)
import Msg exposing (Msg)
import Routing exposing (Route)


type alias Navigation =
    List Item


type alias Item =
    ( String, List Link )


type alias Link =
    { title : String
    , route : Route
    }


bar : String -> Html Msg
bar title =
    Html.nav
        [ class "navbar navbar-default navbar-inverse"
        , attribute "role" "navigation"
        ]
        [ div [ class "container-fluid" ]
            [ div [ class "navbar-header" ]
                [ button
                    [ type_ "button"
                    , class "navbar-toggle collapsed"
                    , attribute "data-toggle" "collapse"
                    , attribute "data-target" "#bs-example-navbar-collapse-1"
                    , attribute "aria-expanded" "false"
                    ]
                    [ span [ class "sr-only" ] [ text "Toggle navigation" ]
                    , span [ class "icon-bar" ] []
                    , span [ class "icon-bar" ] []
                    , span [ class "icon-bar" ] []
                    ]
                , linkTo Routing.Home [ class "navbar-brand" ] [ text title ]
                ]
            , div
                [ class "collapse navbar-collapse", id "bs-example-navbar-collapse-1" ]
                [ ul [ class "nav navbar-nav" ]
                    (List.map renderItem menu)
                ]
            ]
        ]


menu : Navigation
menu =
    [ ( "Section 1", [ { title = "Setion 1.1", route = Routing.Home } ] )
    , ( "Section 2", [ { title = "Section 2.1", route = Routing.About } ] )
    ]


renderItem : Item -> Html Msg
renderItem ( title, links ) =
    li [ class "dropdown" ]
        [ a [ href "#", class "dropdown-toggle", attribute "data-toggle" "dropdown", attribute "role" "button", attribute "aria-haspopup" "true", attribute "aria-expanded" "false" ] [ text title ]
        , ul [ class "dropdown-menu" ] (List.map renderLink links)
        ]


renderLink : Link -> Html Msg
renderLink link =
    li []
        [ linkTo link.route [] [ text link.title ]
        ]
