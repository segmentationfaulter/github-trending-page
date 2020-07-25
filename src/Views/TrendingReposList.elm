module Views.TrendingReposList exposing (TrendingReposList, trendingReposListDecoder, trendingReposView)

import Json.Decode as Decode
import Browser.Dom exposing (Element)
import Element as El exposing (Element, column, el, fillPortion, row, text)

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
        [
            El.width El.fill
        ]
        <| trendingReposList repos

trendingRepoItem : TrendingRepo -> Element msg
trendingRepoItem repo =
    let
        repoTitle : Element msg
        repoTitle =
            el
                []
                <| text (repo.author ++ "/" ++ repo.name)
    in
    
    row
        []
        [
            column
                []
                [
                    repoTitle
                ]
        ]



trendingReposList : TrendingReposList -> List (Element msg)
trendingReposList repos =
    List.map trendingRepoItem repos