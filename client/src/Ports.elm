port module Ports exposing (alert, beep, log)

{-| Play a beep sound with specified frequency.
The code for this lives in main.ts.
-}


port beep : Float -> Cmd msg


port alert : String -> Cmd msg


port log : String -> Cmd msg
