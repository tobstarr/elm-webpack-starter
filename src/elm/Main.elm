module Main exposing (..)

-- component import example

import Components.Hello exposing (hello)
import Helpers exposing (linkTo)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (defaultOptions, onClick, onWithOptions)
import Msg exposing (Msg)
import Navi
import Navigation
import Routing exposing (Route)


-- APP


main : Program Never Model Msg
main =
    Navigation.program Msg.OnLocationChange
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Msg.ChangeLocation path ->
            ( model, Navigation.newUrl path )

        Msg.OnLocationChange location ->
            handleLocation model location

        Msg.Increment ->
            { model | changes = model.changes + 1 } ! []



-- VIEW
-- Html is defined as: elem [ attribs ][ children ]
-- CSS can be applied via class names or inline style attrib


view : Model -> Html Msg
view model =
    case model.route of
        Routing.About ->
            layout
                [ h2 [] [ text "About" ]
                ]

        Routing.Home ->
            index model

        Routing.NotFound ->
            layout [ h1 [] [ text "Not Found" ] ]


index : Model -> Html Msg
index model =
    layout
        [ div []
            [ img [ src "static/img/elm.jpg", style styles.img ] [] -- inline CSS (via var)
            , hello model.changes -- ext 'hello' component (takes 'model' as arg)
            , p [] [ text "Elm Webpack Starter" ]
            , button [ class "btn btn-primary btn-lg", onClick Msg.Increment ]
                [ -- click handler
                  span [ class "glyphicon glyphicon-star" ] [] -- glyphicon
                , span [] [ text "FTW!" ]
                ]
            , div []
                [ linkTo Routing.About [] [ text "About" ] ]
            ]
        ]


layout : List (Html Msg) -> Html Msg
layout a =
    div
        [ class "container"
        , style
            [ ( "margin-top", "30px" )
            , ( "text-align", "center" )
            ]
        ]
        [ Navi.bar "Elm Bootstrap"
        , div [ class "row" ]
            [ div [ class "col-xs-12" ]
                a
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
    handleLocation initialModel location


handleLocation : Model -> Navigation.Location -> ( Model, Cmd Msg )
handleLocation model location =
    let
        route =
            Routing.parseLocation location
    in
    ( { model | route = route }, Cmd.none )


initialModel : Model
initialModel =
    { route = Routing.Home
    , changes = 0
    }
