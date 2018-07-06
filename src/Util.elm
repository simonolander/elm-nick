module Util exposing (..)


import Keyboard exposing (KeyCode)
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


chain : (a -> (a, List b)) -> (a, List (List b)) -> (a, List (List b))
chain func (a, list) =
    let
        (a_, list_) = func a
    in
        (a_, list_ :: list)


noCmd : (a -> a) -> (a, List (List b)) -> (a, List (List b))
noCmd func (a, list) =
    (func a, list)


batch : (a, List (List (Cmd b))) -> (a, Cmd b)
batch (a, list) =
    (a, Cmd.batch (List.concat list))


keyCodeToString : KeyCode -> String
keyCodeToString keyCode =
    case keyCode of
        0 -> "That key has no keycode"
        3 -> "break"
        8 -> "backspace"
        9 -> "tab"
        12 -> "clear"
        13 -> "enter"
        16 -> "shift"
        17 -> "ctrl"
        18 -> "alt"
        19 -> "pause/break"
        20 -> "caps lock"
        21 -> "hangul"
        25 -> "hanja"
        27 -> "escape"
        28 -> "conversion"
        29 -> "non-conversion"
        32 -> "space"
        33 -> "page up"
        34 -> "page down"
        35 -> "end"
        36 -> "home"
        37 -> "left"
        38 -> "up"
        39 -> "right"
        40 -> "down"
        41 -> "select"
        42 -> "print"
        43 -> "execute"
        44 -> "Print Screen"
        45 -> "insert"
        46 -> "delete"
        47 -> "help"
        48 -> "0"
        49 -> "1"
        50 -> "2"
        51 -> "3"
        52 -> "4"
        53 -> "5"
        54 -> "6"
        55 -> "7"
        56 -> "8"
        57 -> "9"
        58 -> ":"
        59 -> "semicolon (firefox), equals"
        60 -> "<"
        61 -> "equals (firefox)"
        63 -> "ß"
        64 -> "@ (firefox)"
        65 -> "a"
        66 -> "b"
        67 -> "c"
        68 -> "d"
        69 -> "e"
        70 -> "f"
        71 -> "g"
        72 -> "h"
        73 -> "i"
        74 -> "j"
        75 -> "k"
        76 -> "l"
        77 -> "m"
        78 -> "n"
        79 -> "o"
        80 -> "p"
        81 -> "q"
        82 -> "r"
        83 -> "s"
        84 -> "t"
        85 -> "u"
        86 -> "v"
        87 -> "w"
        88 -> "x"
        89 -> "y"
        90 -> "z"
        91 -> "left meta"
        92 -> "right window key"
        93 -> "right meta"
        95 -> "sleep"
        96 -> "numpad 0"
        97 -> "numpad 1"
        98 -> "numpad 2"
        99 -> "numpad 3"
        100 -> "numpad 4"
        101 -> "numpad 5"
        102 -> "numpad 6"
        103 -> "numpad 7"
        104 -> "numpad 8"
        105 -> "numpad 9"
        106 -> "multiply"
        107 -> "add"
        108 -> "numpad period (firefox)"
        109 -> "subtract"
        110 -> "decimal point"
        111 -> "divide"
        112 -> "f1"
        113 -> "f2"
        114 -> "f3"
        115 -> "f4"
        116 -> "f5"
        117 -> "f6"
        118 -> "f7"
        119 -> "f8"
        120 -> "f9"
        121 -> "f10"
        122 -> "f11"
        123 -> "f12"
        124 -> "f13"
        125 -> "f14"
        126 -> "f15"
        127 -> "f16"
        128 -> "f17"
        129 -> "f18"
        130 -> "f19"
        131 -> "f20"
        132 -> "f21"
        133 -> "f22"
        134 -> "f23"
        135 -> "f24"
        144 -> "num lock"
        145 -> "scroll lock"
        160 -> "^"
        161 -> "!"
        162 -> "arabic semicolon"
        163 -> "#"
        164 -> "$"
        165 -> "ù"
        166 -> "page backward"
        167 -> "page forward"
        168 -> "refresh"
        169 -> "closing paren (AZERTY)"
        170 -> "*"
        171 -> "~ + * key"
        172 -> "home key"
        173 -> "minus (firefox), mute/unmute"
        174 -> "decrease volume level"
        175 -> "increase volume level"
        176 -> "next"
        177 -> "previous"
        178 -> "stop"
        179 -> "play/pause"
        180 -> "e-mail"
        181 -> "mute/unmute (firefox)"
        182 -> "decrease volume level (firefox)"
        183 -> "increase volume level (firefox)"
        186 -> "ö"
        187 -> "+ / `"
        188 -> "comma"
        189 -> "dash"
        190 -> "period"
        191 -> "forward slash / ç"
        192 -> "§"
        193 -> "?, / or °"
        194 -> "numpad period (chrome)"
        219 -> "å"
        220 -> "back slash"
        221 -> "¨"
        222 -> "ä / *"
        223 -> "`"
        224 -> "left or right ⌘ key (firefox)"
        225 -> "altgr"
        226 -> "< /git >, left back slash"
        230 -> "GNOME Compose Key"
        231 -> "ç"
        233 -> "XF86Forward"
        234 -> "XF86Back"
        235 -> "non-conversion"
        240 -> "alphanumeric"
        242 -> "hiragana/katakana"
        243 -> "half-width/full-width"
        244 -> "kanji"
        251 -> "unlock trackpad (Chrome/Edge)"
        255 -> "toggle touchpad"
        other -> "Unknown: " ++ toString other
