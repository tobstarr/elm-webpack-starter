module Main exposing (..)

-- component import example

import Components.Hello exposing (hello)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (defaultOptions, onClick, onWithOptions)
import Json.Decode
import Navigation
import UrlParser


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
            ( { model | changes = model.changes + 1 }, Navigation.newUrl path )

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
            div [] [ h2 [] [ text "about" ] ]

        _ ->
            div [ class "container", style [ ( "margin-top", "30px" ), ( "text-align", "center" ) ] ]
                [ -- inline CSS (literal)
                  div [ class "row" ]
                    [ div [ class "col-xs-12" ]
                        [ div [ class "jumbotron" ]
                            [ img [ src "static/img/elm.jpg", style styles.img ] [] -- inline CSS (via var)
                            , hello model.changes -- ext 'hello' component (takes 'model' as arg)
                            , p [] [ text "Elm Webpack Starter" ]
                            , button [ class "btn btn-primary btn-lg", onClick Increment ]
                                [ -- click handler
                                  span [ class "glyphicon glyphicon-star" ] [] -- glyphicon
                                , span [] [ text "FTW!" ]
                                ]
                            , div []
                                [ linkTo AboutRoute [ text "About" ] ]
                            ]
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


matchers : UrlParser.Parser (Route -> a) a
matchers =
    UrlParser.oneOf
        [ UrlParser.map HomeRoute UrlParser.top
        , UrlParser.map AboutRoute (UrlParser.s "about")
        ]


homePath =
    "/"


aboutPath =
    "/about"


initialModel : Route -> Model
initialModel route =
    { route = route
    , changes = 0
    }


linkTo : Route -> List (Html Msg) -> Html Msg
linkTo route inner =
    let
        path =
            case route of
                AboutRoute ->
                    "/about"

                HomeRoute ->
                    "/"

                NotFoundRoute ->
                    "/"
    in
    a [ href "/changes", onPreventDefaultClick (ChangeLocation path) ] inner


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
