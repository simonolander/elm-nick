module Util exposing (..)


maybeCons : Maybe a -> List a -> List a
maybeCons maybe list =
    case maybe of
        Just value ->
            value :: list
        Nothing ->
            list


filterMaybe : List (Maybe a) -> List a
filterMaybe = List.foldr maybeCons []
