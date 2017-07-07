module Main exposing (..)

-- component import example

import Components.Hello exposing (hello)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (defaultOptions, onClick, onWithOptions)
import Json.Decode
import Navigation
import UrlParser exposing ((</>))


-- APP


main : Program Never Model Msg
main =
    Navigation.program OnLocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { changes : Int
    , route : Route
    }



-- UPDATE


type Msg
    = ChangeLocation String
    | OnLocationChange Navigation.Location
    | Increment


type Route
    = HomeRoute
    | AboutRoute
    | NotFoundRoute


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeLocation path ->
            ( model, Navigation.newUrl path )

        OnLocationChange location ->
            let
                newRoute =
                    parseLocation location
            in
            ( { model | route = newRoute }, Cmd.none )

        Increment ->
            { model | changes = model.changes + 1 } ! []



-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names or inline style attrib


view : Model -> Html Msg
view model =
    case model.route of
        AboutRoute ->
            layout
                [ h2 [] [ text "about" ]
                ]

        HomeRoute ->
            index model

        NotFoundRoute ->
            layout [ h1 [] [ text "Not Found" ] ]


index model =
    layout
        [ div []
            [ img [ src "static/img/elm.jpg", style styles.img ] [] -- inline CSS (via var)
            , hello model.changes -- ext 'hello' component (takes 'model' as arg)
            , p [] [ text "Elm Webpack Starter" ]
            , button [ class "btn btn-primary btn-lg", onClick Increment ]
                [ -- click handler
                  span [ class "glyphicon glyphicon-star" ] [] -- glyphicon
                , span [] [ text "FTW!" ]
                ]
            , div []
                [ linkTo AboutRoute [] [ text "About" ] ]
            ]
        ]


layout a =
    div
        [ class "container"
        , style
            [ ( "margin-top", "30px" )
            , ( "text-align", "center" )
            ]
        ]
        [ navbar
        , div [ class "row" ]
            [ div [ class "col-xs-12" ]
                a
            ]
        ]


title =
    "Elm Bootstrap"


navigationItem ne =
    li [ class "dropdown" ]
        [ a [ href "#", class "dropdown-toggle", attribute "data-toggle" "dropdown", attribute "role" "button", attribute "aria-haspopup" "true", attribute "aria-expanded" "false" ] [ text ne.title ]
        , ul [ class "dropdown-menu" ] (List.map navigationLink ne.links)
        ]


type alias NavigationLink =
    { title : String
    , route : Route
    }


navigationLink : NavigationLink -> Html Msg
navigationLink link =
    li []
        [ linkTo link.route [] [ text link.title ]
        ]


navbar =
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
                , linkTo HomeRoute [ class "navbar-brand" ] [ text title ]
                ]
            , div
                [ class "collapse navbar-collapse", id "bs-example-navbar-collapse-1" ]
                [ ul [ class "nav navbar-nav" ]
                    (List.map navigationItem navigationMenu)
                ]
            ]
        ]



-- CSS STYLES


styles : { img : List ( String, String ) }
styles =
    { img =
        [ ( "width", "33%" )
        , ( "border", "4px solid #337AB7" )
        ]
    }


navigationMenu =
    [ { title = "Section 1"
      , links =
            [ { title = "Setion 1.1", route = HomeRoute }
            ]
      }
    , { title = "Section 2"
      , links =
            [ { title = "Section 2.1", route = AboutRoute }
            ]
      }
    ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        currentRoute =
            parseLocation location
    in
    ( initialModel currentRoute, Cmd.none )


parseLocation : Navigation.Location -> Route
parseLocation location =
    case UrlParser.parsePath matchers location of
        Just route ->
            route

        Nothing ->
            NotFoundRoute


routes =
    { user = "user"
    , about = "about"
    }


matchers : UrlParser.Parser (Route -> a) a
matchers =
    UrlParser.oneOf
        [ UrlParser.map HomeRoute UrlParser.top
        , UrlParser.map AboutRoute (UrlParser.s routes.about)
        ]


initialModel : Route -> Model
initialModel route =
    { route = route
    , changes = 0
    }


linkTo : Route -> List (Attribute Msg) -> List (Html Msg) -> Html Msg
linkTo route atts inner =
    let
        path =
            routeFor route

        linkAtts =
            atts ++ [ href path, onPreventDefaultClick (ChangeLocation path) ]
    in
    a linkAtts inner


routeFor route =
    let
        r list =
            "/" ++ String.join "/" list
    in
    case route of
        AboutRoute ->
            r [ routes.about ]

        HomeRoute ->
            r [ "" ]

        NotFoundRoute ->
            r [ "" ]


onPreventDefaultClick : msg -> Attribute msg
onPreventDefaultClick message =
    onWithOptions "click"
        { defaultOptions | preventDefault = True }
        (preventDefault2
            |> Json.Decode.andThen (maybePreventDefault message)
        )


preventDefault2 : Json.Decode.Decoder Bool
preventDefault2 =
    Json.Decode.map2
        invertedOr
        (Json.Decode.field "ctrlKey" Json.Decode.bool)
        (Json.Decode.field "metaKey" Json.Decode.bool)


maybePreventDefault : msg -> Bool -> Json.Decode.Decoder msg
maybePreventDefault msg preventDefault =
    case preventDefault of
        True ->
            Json.Decode.succeed msg

        False ->
            Json.Decode.fail "Normal link"


invertedOr : Bool -> Bool -> Bool
invertedOr x y =
    not (x || y)
