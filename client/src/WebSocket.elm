port module WebSocket exposing (ClientToServer(..), ExtinctionSettings, PerformanceData, ServerToClient(..), Settings, receive, send)

import Fragment
import Json.Decode as JsonD
import Json.Encode as JsonE



---- Internal JavaScript ports ----
{- JavaScript ports only allow certain types, like Int, String or JSON.
   JSON is handy, since we can encode and decode our Elm types to JSON.
-}


port internalSend : JsonE.Value -> Cmd msg


port internalReceive : (JsonE.Value -> msg) -> Sub msg



---- Elm ports ----
{- These "ports" are just wrappers around the JavaScript ports,
   with the encoding and decoding already done!
-}


send : ClientToServer -> Cmd msg
send =
    encode >> internalSend


receive : (ServerToClient -> msg) -> Sub msg
receive handler =
    internalReceive
        (\val ->
            case JsonD.decodeValue decode val of
                Ok stc ->
                    handler stc

                Err err ->
                    handler (Error <| JsonD.errorToString err)
        )



---- Client to Server messages ----


type ClientToServer
    = CreateLobbyRequest
    | JoinLobbyRequest { pin : Int }
    | LobbySetReady { pin : Int }
    | LeaveLobbyRequest { pin : Int }
    | DisbandLobbyRequest { pin : Int }
    | StartPerformanceRequest { pin : Int, settings : Settings }
    | SendHeartbeat { pin : Int, fragment : Fragment.Timeseries }


type alias Settings =
    { bpm : Int
    , extinction : ExtinctionSettings
    }


type alias ExtinctionSettings =
    { events : Int
    , probability : Int -- Percentages
    , rate : Int -- Percentages
    }



---- Server to Client messages ----


type ServerToClient
    = Error String
    | CreateLobbyResponse LobbyResponseData
    | LobbyUpdate LobbyResponseData
    | LobbyDisbanded { pin : Int }
    | StartPerformanceResponse PerformanceData
    | ReceiveHeartbeat { fragment : Fragment.Timeseries }


type alias LobbyResponseData =
    { pin : Int
    , readyPlayers : Int
    , totalPlayers : Int
    }


type alias PerformanceData =
    { pin : Int
    , seed : Int
    , start : Int
    , bpm : Int
    , programme : List { generation : Int, extinct : Bool }
    }



---- Client to Server JSON encoders ----
{- Note: It is OCaml convention to use capitalised snake case.
   The server side derives JSON automatically, so we follow that format.
-}


{-| Encode Client to Server messages in JSON.
-}
encode : ClientToServer -> JsonE.Value
encode msg =
    case msg of
        CreateLobbyRequest ->
            JsonE.list identity [ JsonE.string "Create_lobby_request" ]

        JoinLobbyRequest { pin } ->
            encodeVariant "Join_lobby_request" [ ( "pin", JsonE.int pin ) ]

        LobbySetReady { pin } ->
            encodeVariant "Lobby_set_ready" [ ( "pin", JsonE.int pin ) ]

        LeaveLobbyRequest { pin } ->
            encodeVariant "Leave_lobby_request" [ ( "pin", JsonE.int pin ) ]

        DisbandLobbyRequest { pin } ->
            encodeVariant "Disband_lobby_request" [ ( "pin", JsonE.int pin ) ]

        StartPerformanceRequest { pin, settings } ->
            encodeVariant "Start_performance_request"
                [ ( "pin", JsonE.int pin )
                , ( "settings"
                  , JsonE.object
                        [ ( "bpm", JsonE.int settings.bpm )
                        , ( "extinction"
                          , JsonE.object
                                [ ( "events", JsonE.int settings.extinction.events )
                                , ( "probability", JsonE.int settings.extinction.probability )
                                , ( "rate", JsonE.int settings.extinction.rate )
                                ]
                          )
                        ]
                  )
                ]

        SendHeartbeat { pin, fragment } ->
            encodeVariant "Send_heartbeat"
                [ ( "pin", JsonE.int pin )
                , ( "fragment", Fragment.encodeTS fragment )
                ]


{-| Encode a sum type in the same way OCaml Yojson expects it:
an array of the variant name, cons its arguments.
-}
encodeVariant : String -> List ( String, JsonE.Value ) -> JsonE.Value
encodeVariant name fields =
    JsonE.list identity [ JsonE.string name, JsonE.object fields ]



---- Server to Client JSON decoders ----


{-| Decode the variant name (the first element in the array),
then decode the second element based on the result of the first.
-}
decode : JsonD.Decoder ServerToClient
decode =
    JsonD.index 0 JsonD.string |> JsonD.andThen (JsonD.index 1 << decodeByVariant)


decodeByVariant : String -> JsonD.Decoder ServerToClient
decodeByVariant variant =
    case variant of
        "Error" ->
            JsonD.map Error JsonD.string

        "Create_lobby_response" ->
            JsonD.map CreateLobbyResponse decodeLobbyResponseData

        "Lobby_update" ->
            JsonD.map LobbyUpdate decodeLobbyResponseData

        "Lobby_disbanded" ->
            JsonD.map (\pin -> LobbyDisbanded { pin = pin }) (JsonD.field "pin" JsonD.int)

        "Start_performance_response" ->
            JsonD.map StartPerformanceResponse <|
                JsonD.map5 PerformanceData
                    (JsonD.field "pin" JsonD.int)
                    (JsonD.field "seed" JsonD.int)
                    (JsonD.field "start" JsonD.int)
                    (JsonD.field "bpm" JsonD.int)
                    (JsonD.field "programme"
                        (JsonD.list <|
                            JsonD.map2
                                (\generation extinct -> { generation = generation, extinct = extinct })
                                (JsonD.field "generation" JsonD.int)
                                (JsonD.field "extinct" JsonD.bool)
                        )
                    )

        "Receive_heartbeat" ->
            JsonD.map
                (\fragment -> ReceiveHeartbeat { fragment = fragment })
                (JsonD.field "fragment" Fragment.decodeTS)

        _ ->
            JsonD.fail ("Unknown message variant: " ++ variant)


decodeLobbyResponseData : JsonD.Decoder LobbyResponseData
decodeLobbyResponseData =
    JsonD.map3 LobbyResponseData
        (JsonD.field "pin" JsonD.int)
        (JsonD.field "ready_players" JsonD.int)
        (JsonD.field "total_players" JsonD.int)
