module Views.TrendingReposList exposing (TrendingReposList, trendingReposListDecoder, trendingReposView)

import Browser.Dom exposing (Element)
import Element as El exposing (Element, column, el, fillPortion, row, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Hex exposing (fromString)
import Json.Decode as Decode
import Views.Icons as Icons


type alias TrendingRepo =
    { author : String
    , name : String
    , url : String
    , description : String
    , language : Maybe String
    , languageColor : Maybe String
    , stars : Int
    , forks : Int
    , starsToday : Int
    }


type alias TrendingReposList =
    List TrendingRepo


trendingReposListDecoder : Decode.Decoder TrendingReposList
trendingReposListDecoder =
    let
        trendingRepoDecoder : Decode.Decoder TrendingRepo
        trendingRepoDecoder =
            let
                authorDecoder =
                    Decode.field "author" Decode.string

                nameDecoder =
                    Decode.field "name" Decode.string

                urlDecoder =
                    Decode.field "url" Decode.string

                descriptionDecoder =
                    Decode.field "description" Decode.string

                languageDecoder =
                    Decode.maybe (Decode.field "language" Decode.string)

                languageColorDecoder =
                    Decode.maybe (Decode.field "languageColor" Decode.string)

                starsDecoder =
                    Decode.field "stars" Decode.int

                forksDecoder =
                    Decode.field "forks" Decode.int

                starsTodayDecoder =
                    Decode.field "currentPeriodStars" Decode.int
            in
            Decode.map2
                (\a b -> { author = a.author, name = a.name, url = a.url, description = a.description, language = a.language, languageColor = a.languageColor, stars = b.stars, forks = b.forks, starsToday = b.starsToday })
                (Decode.map6
                    (\author name url description language languageColor -> { author = author, name = name, url = url, description = description, language = language, languageColor = languageColor })
                    authorDecoder
                    nameDecoder
                    urlDecoder
                    descriptionDecoder
                    languageDecoder
                    languageColorDecoder
                )
                (Decode.map3
                    (\stars forks starsToday -> { stars = stars, forks = forks, starsToday = starsToday })
                    starsDecoder
                    forksDecoder
                    starsTodayDecoder
                )
    in
    Decode.list trendingRepoDecoder


trendingReposView : TrendingReposList -> Element msg
trendingReposView repos =
    column
        [ El.width El.fill
        ]
    <|
        trendingReposList repos


trendingRepoItem : TrendingRepo -> Element msg
trendingRepoItem repo =
    let
        repoTitle : Element msg
        repoTitle =
            row
                [ Font.color <| El.rgb255 3 102 214 ]
                [ el
                    []
                  <|
                    text (repo.author ++ "/")
                , el
                    [ Font.semiBold ]
                  <|
                    text repo.name
                ]

        repoDescription : Element msg
        repoDescription =
            El.paragraph
                [ Font.alignLeft
                , Font.size 14
                , El.spacing 7
                , Font.color <| El.rgb255 88 96 105
                ]
                [ text repo.description ]

        languageInfo : Element msg
        languageInfo =
            case repo.language of
                Maybe.Just language ->
                    case repo.languageColor of
                        Maybe.Just languageColor ->
                            let
                                rgbColorResult : Result String El.Color
                                rgbColorResult =
                                    convertFromHexToRgbColor languageColor
                            in
                            case rgbColorResult of
                                Result.Ok color ->
                                    row
                                        [ El.spacing 5 ]
                                        [ el
                                            [ El.width <| El.px 12
                                            , El.height <| El.px 12
                                            , Background.color color
                                            , Border.rounded 50
                                            ]
                                            El.none
                                        , text language
                                        ]

                                Err _ ->
                                    El.none

                        Maybe.Nothing ->
                            El.none

                Maybe.Nothing ->
                    El.none

        bottomLeftRow : Element msg
        bottomLeftRow =
            row
                [ Font.color <| El.rgb255 88 96 105, Font.size 12, El.width El.fill, El.spacing 16 ]
                [ languageInfo
                , El.row [] [ Icons.starIcon, text <| " " ++ String.fromInt repo.stars ]
                , El.row [] [ Icons.forkIcon, text <| " " ++ String.fromInt repo.forks ]
                ]

        starsToday : Element msg
        starsToday =
            El.paragraph
                [Font.color <| El.rgb255 88 96 105, Font.size 12]
                [
                    Icons.starIcon,
                    text <| " " ++ String.fromInt repo.starsToday ++ " stars today"
                ]
    in
    row
        [ El.width El.fill
        , El.padding 16
        , Border.widthEach { left = 1, bottom = 1, right = 1, top = 0 }
        , Border.color <| El.rgb255 225 228 232
        ]
        [ column
            [ El.width <| El.fillPortion 8, El.spacing 8 ]
            [ El.newTabLink [] { url = repo.url, label = repoTitle }
            , repoDescription
            , bottomLeftRow
            ]
        , el [ El.width <| El.fillPortion 2, El.alignBottom ] starsToday
        ]


trendingReposList : TrendingReposList -> List (Element msg)
trendingReposList repos =
    List.map trendingRepoItem repos


convertFromHexToRgbColor : String -> Result String El.Color
convertFromHexToRgbColor hexColor =
    let
        colorWithoutHash : String
        colorWithoutHash =
            String.dropLeft 1 hexColor

        r : String
        r =
            String.left 2 colorWithoutHash

        g : String
        g =
            String.slice 2 4 colorWithoutHash

        b : String
        b =
            String.right 2 colorWithoutHash

        rInt : Result String Int
        rInt =
            fromString r

        gInt : Result String Int
        gInt =
            fromString g

        bInt : Result String Int
        bInt =
            fromString b
    in
    Result.map3
        (\rPart gPart bPart -> El.rgb255 rPart gPart bPart)
        rInt
        gInt
        bInt
