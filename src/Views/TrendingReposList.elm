module Views.TrendingReposList exposing(TrendingRepo, TrendingReposList, trendingReposListDecoder)
import Json.Decode as Decode

type alias TrendingRepo =
    { author : String
    , name : String
    , url : String
    , description : String
    , language : String
    , languageColor : String
    , stars : Int
    , forks : Int
    , starsToday : Int
    }

type alias TrendingReposList = List TrendingRepo

trendingReposListDecoder : Decode.Decoder TrendingReposList
trendingReposListDecoder =
    let
        trendingRepoDecoder : Decode.Decoder TrendingRepo
        trendingRepoDecoder =
            let
                authorDecoder = Decode.field "author" Decode.string
                nameDecoder = Decode.field "name" Decode.string
                urlDecoder = Decode.field "url" Decode.string
                descriptionDecoder = Decode.field "description" Decode.string
                languageDecoder = Decode.field "language" Decode.string
                languageColorDecoder = Decode.field "languageColor" Decode.string
                starsDecoder = Decode.field "stars" Decode.int
                forksDecoder = Decode.field "forks" Decode.int
                starsTodayDecoder = Decode.field "currentPeriodStars" Decode.int
            in
                Decode.map2
                    (\a b -> { author = a.uthor, name = a.name, url = a.url, description = a.description, language = a.language, languageColor = a.languageColor})
                    Decode.map6
                        (\author name url description language languageColor -> { author = author, name = name, url = url, description = description, language = language, languageColor = languageColor})
                        authorDecoder nameDecoder urlDecoder descriptionDecoder languageDecoder languageColorDecoder 
                    Decode.map3
                        (\stars forks starsToday -> {stars = stars, forks = forks, starsToday = starsToday })
        in
            Decode.list trendingRepoDecoder