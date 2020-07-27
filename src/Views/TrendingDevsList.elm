module Views.TrendingDevsList exposing (TrendingDevsList, trendingDevsListDecoder)

import Json.Decode as Decode


type alias TrendingDev =
    { username : String
    , name : String
    , url : String
    , sponsorUrl : Maybe String
    , avatar : String
    , repo : PopularRepo
    }


type alias PopularRepo =
    { name : String
    , description : String
    , url : String
    }


type alias TrendingDevsList =
    List TrendingDev


trendingDevsListDecoder : Decode.Decoder TrendingDevsList
trendingDevsListDecoder =
    let
        usernameDecoder =
            Decode.field "username" Decode.string

        nameDecoder =
            Decode.field "name" Decode.string

        urlDecoder =
            Decode.field "url" Decode.string

        sponsorUrlDecoder =
            Decode.maybe <| Decode.field "sponsorUrl" Decode.string

        avatarDecoder =
            Decode.field "avatar" Decode.string

        repoDecoder : Decode.Decoder PopularRepo
        repoDecoder =
            Decode.field "repo"
                (Decode.map3
                    (\name description url -> { name = name, description = description, url = url })
                    (Decode.field "name" Decode.string)
                    (Decode.field "description" Decode.string)
                    (Decode.field "url" Decode.string)
                )
    in
    Decode.list <| Decode.map6
        (\username name url sponsorUrl avatar repo -> { username = username, name = name, url = url, sponsorUrl = sponsorUrl, avatar = avatar, repo = repo })
        usernameDecoder
        nameDecoder
        urlDecoder
        sponsorUrlDecoder
        avatarDecoder
        repoDecoder