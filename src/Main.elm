module Main exposing (main)

import Browser
import Element as El exposing (Element, column, el, fillPortion, row, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as InputEl
import Element.Region exposing (mainContent)
import Html exposing (Html)
import Http
import Views.Header exposing (headerView)
import Views.TrendingDevsList exposing (TrendingDevsList, trendingDevsListDecoder, trendingDevsView)
import Views.TrendingReposList exposing (TrendingReposList, trendingReposListDecoder, trendingReposView)



---- MODEL ----


type Model
    = InitialLoading
    | ReposView TrendingReposList
    | LoadingDevsView
    | DevsView TrendingDevsList
    | Failure Http.Error


init : ( Model, Cmd Msg )
init =
    ( InitialLoading, fetchTrendingRepos )


fetchTrendingRepos : Cmd Msg
fetchTrendingRepos =
    Http.get
        { url = "https://ghapi.huchen.dev/repositories"
        , expect = Http.expectJson GotTrendingRepos trendingReposListDecoder
        }


fetchTrendingDevs : Cmd Msg
fetchTrendingDevs =
    Http.get
        { url = "https://ghapi.huchen.dev/developers"
        , expect = Http.expectJson GotTrendingDevs trendingDevsListDecoder
        }



---- UPDATE ----


type Msg
    = Noop
    | GotTrendingRepos (Result Http.Error TrendingReposList)
    | SwithchToDevsView
    | GotTrendingDevs (Result Http.Error TrendingDevsList)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotTrendingRepos result ->
            case result of
                Ok trendingRepos ->
                    ( ReposView trendingRepos, Cmd.none )

                Err httpError ->
                    ( Failure httpError, Cmd.none )

        SwithchToDevsView ->
            ( LoadingDevsView, fetchTrendingDevs )

        GotTrendingDevs result ->
            case result of
                Ok trendingDevs ->
                    ( DevsView trendingDevs, Cmd.none )

                Err httpError ->
                    ( Failure httpError, Cmd.none )

        Noop ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        currentView : Element Msg
        currentView =
            let
                loadingScreen : Element Msg
                loadingScreen =
                    el [ Font.size 40, El.centerX, El.centerY ] <| text "Loading..."

                trendingView : Element Msg -> Element Msg
                trendingView selectedView =
                    row
                        [ El.width El.fill
                        ]
                        [ el [ El.width <| fillPortion 1 ] El.none
                        , column
                            [ El.width <| fillPortion 5
                            ]
                            [ trendingViewControls model
                            , selectedView
                            , el [ El.height <| El.px 50 ] El.none
                            ]
                        , el [ El.width <| fillPortion 1 ] El.none
                        ]
            in
            case model of
                InitialLoading ->
                    loadingScreen

                ReposView trendingRepos ->
                    elmUIView <| trendingView <| trendingReposView trendingRepos

                LoadingDevsView ->
                    loadingScreen

                DevsView trendingDevs ->
                    elmUIView <| trendingView <| trendingDevsView trendingDevs

                Failure error ->
                    failureView error
    in
    El.layout [] <| currentView


failureView : Http.Error -> Element Msg
failureView error =
    case error of
        Http.BadUrl errorMsg ->
            el [] <| text errorMsg

        Http.Timeout ->
            el [] <| text "It is taking too long, try refreshing"

        Http.NetworkError ->
            el [] <| text "Looks like you don't have internet"

        Http.BadStatus _ ->
            el [] <| text "We got an error reponse from server"

        Http.BadBody errorMsg ->
            el [] <| text errorMsg


elmUIView : Element Msg -> Element Msg
elmUIView trendingView =
    column
        [ mainContent
        , El.width El.fill
        , El.spacing 40
        ]
        [ headerView
        , trendingView
        ]


trendingViewControls : Model -> Element Msg
trendingViewControls model =
    let
        reposButtonLabel : String
        reposButtonLabel =
            "Repositories"

        devsButtonLabel : String
        devsButtonLabel =
            "Developers"

        renderButton : String -> Msg -> Element Msg
        renderButton label msg =
            let
                isActive : Bool
                isActive =
                    case model of
                        ReposView _ ->
                            label == reposButtonLabel

                        DevsView _ ->
                            label == devsButtonLabel

                        InitialLoading ->
                            False

                        Failure _ ->
                            False

                        LoadingDevsView ->
                            False

                activeStateColor : El.Color
                activeStateColor =
                    El.rgb255 3 102 214

                roundedBorders : El.Attribute Msg
                roundedBorders =
                    if label == reposButtonLabel then
                        Border.roundEach { topLeft = 6, bottomLeft = 6, topRight = 0, bottomRight = 0 }

                    else
                        Border.roundEach { topLeft = 0, bottomLeft = 0, topRight = 6, bottomRight = 6 }

                borderWidth : El.Attribute Msg
                borderWidth =
                    if isActive then
                        Border.width 1

                    else
                        case label of
                            "Repositories" ->
                                Border.widthEach { left = 1, top = 1, bottom = 1, right = 0 }

                            _ ->
                                Border.widthEach { left = 0, top = 1, bottom = 1, right = 1 }

                borderColor : El.Attribute Msg
                borderColor =
                    if isActive then
                        Border.color activeStateColor

                    else
                        Border.color <| El.rgb255 225 228 232

                backgroundColor : El.Color
                backgroundColor =
                    if isActive then
                        activeStateColor

                    else
                        El.rgb255 246 248 250

                fontColor : El.Color
                fontColor =
                    if isActive then
                        El.rgb255 255 255 255

                    else
                        El.rgb255 0 0 0
            in
            InputEl.button
                [ Background.color backgroundColor
                , El.paddingXY 16 5
                , Font.size 14
                , Font.color fontColor
                , roundedBorders
                , borderWidth
                , borderColor
                , El.height <| El.px 32
                ]
                { onPress = Just msg, label = text label }
    in
    row
        [ Border.width 1
        , Border.color <| El.rgb255 225 228 232
        , Border.roundEach { topLeft = 6, topRight = 6, bottomLeft = 0, bottomRight = 0 }
        , El.width El.fill
        , El.height <| El.px 66
        , El.padding 16
        , Background.color <| El.rgb255 246 248 250
        ]
        [ renderButton reposButtonLabel Noop
        , renderButton devsButtonLabel SwithchToDevsView
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
