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


separate : (a -> Bool) -> List a -> (List a, List a)
separate func list =
    case list of
        head :: tail ->
            let
                (true, false) = separate func tail
            in
                if func head then
                    (head :: true, false)
                else
                    (true, head :: false)

        [] -> ([], [])
