module Rest exposing (getScores, postScore)

import Http
import Model exposing (..)
import RemoteData
import Json.Decode exposing (Decoder)
import Json.Encode


{-| The url to our web server where we host the scores
-}
baseUrl : String
baseUrl = "https://97oxr397sj.execute-api.us-west-2.amazonaws.com/staging/scores"


{-| A json decoder is a function-ish that lets us extract data form json strings.
    This one produces a Score if the json is like {"score": 3, "name": "anon"},
    or otherwise it returns some kind of error.
-}
scoreDecoder : Json.Decode.Decoder Score
scoreDecoder =
    let
        toScore : Int -> String -> Score
        toScore score username = { score = score, username = username }
    in
    Json.Decode.map2
        toScore
        (Json.Decode.field "score" Json.Decode.int)
        (Json.Decode.field "username" Json.Decode.string)



{-| Posts a score to the server, and returns a list of the new scores.
-}
postScore : GameMode -> Score -> Cmd Msg
postScore gameMode { score, username } =
    let
        game =
            "elm-nick " ++ (toString gameMode)

        object =
            Json.Encode.object
                [ ("score", Json.Encode.int score)
                , ("username", Json.Encode.string username)
                , ("game", Json.Encode.string game)
                ]
        body =
            Http.jsonBody
                object

        url =
            baseUrl
    in
        Http.post url body (Json.Decode.list scoreDecoder)
            |> RemoteData.sendRequest
            |> Cmd.map ReceiveScores

{-| Gets the scores from the server and decodes them into a List Score
    packed into a ReceiveScores message.
-}
getScores : Cmd Msg
getScores =
    Http.get (baseUrl ++ "?game=elm-nick") (Json.Decode.list scoreDecoder)
        |> RemoteData.sendRequest
        |> Cmd.map ReceiveScores
