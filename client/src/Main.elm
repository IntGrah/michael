module Main exposing (main)

import Browser
import Browser.Dom
import Char
import Element exposing (Color, Element, alignRight, centerX, centerY, column, el, fill, focusStyle, height, image, maximum, mouseOver, moveLeft, padding, paddingEach, paragraph, px, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import Html exposing (Html)
import Html.Attributes as HtmlA
import Performance exposing (Msg(..))
import Ports
import Task
import Theme exposing (backgroundTransition, green, grey2, grey3, grey5, grey6, grey9, red, white, whitesmoke, withAlpha)
import WebSocket as WS



---- Model ----


{-| A variant for each page.
-}
type Model
    = Home { pinDraft : String }
    | About
    | Tutorial
    | Lobby LobbyModel
    | Performance Performance.Model


type alias LobbyModel =
    { pin : Int
    , instruments : Int
    , ready : Bool
    , readyPlayers : Int
    , totalPlayers : Int

    -- Only the manager can see and modify the settings
    , manager : Maybe WS.Settings
    }



---- Init ----


init : Model
init =
    Home { pinDraft = "" }



---- Update ----


type Msg
    = ---- Generic messages
      Navigate Model
    | WS WS.ServerToClient
      ---- Home messages
    | CreateLobbyRequest -- Either create a lobby,
    | PinDraftChanged String -- or type in a pin
    | JoinLobbyRequest -- and join a lobby
      ---- Lobby messages
    | InstrumentsChanged Int
    | SetReady
    | LeaveLobby -- If you're the manager, this also means "disband the lobby"
    | SettingsDraftChanged WS.Settings
    | StartPerformance
      ---- Performance messages
    | PerformanceMsg Performance.Msg -- Delegate to the Performance module to handle these


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case ( message, model ) of
        ---- Generic messages
        ( Navigate m, _ ) ->
            ( m, Cmd.none )

        ( WS (WS.Error err), _ ) ->
            ( model, Ports.log err )

        ---- Home messages
        ( CreateLobbyRequest, Home _ ) ->
            ( model, WS.send WS.CreateLobbyRequest )

        ( WS (WS.CreateLobbyResponse d), Home _ ) ->
            ( Lobby
                { pin = d.pin
                , ready = True
                , readyPlayers = d.readyPlayers
                , totalPlayers = d.totalPlayers
                , instruments = 5
                , manager =
                    Just
                        { bpm = 90
                        , extinction =
                            { events = 3
                            , probability = 75
                            , rate = 2
                            }
                        }
                }
            , Cmd.none
            )

        ( PinDraftChanged draft, Home m ) ->
            ( Home { m | pinDraft = String.filter Char.isDigit draft |> String.slice 0 6 }, Cmd.none )

        ( JoinLobbyRequest, Home m ) ->
            case String.toInt m.pinDraft of
                Nothing ->
                    ( model, Cmd.none )

                Just pin ->
                    ( model, WS.send (WS.JoinLobbyRequest { pin = pin }) )

        ( WS (WS.LobbyUpdate d), Home _ ) ->
            -- Joining a lobby
            ( Lobby
                { pin = d.pin
                , ready = False
                , readyPlayers = d.readyPlayers
                , totalPlayers = d.totalPlayers
                , instruments = 5
                , manager = Nothing
                }
            , Cmd.none
            )

        ---- Lobby messages
        ( SetReady, Lobby m ) ->
            ( Lobby { m | ready = True }, WS.send (WS.LobbySetReady { pin = m.pin }) )

        ( InstrumentsChanged instruments, Lobby m ) ->
            ( Lobby { m | instruments = instruments }, Cmd.none )

        ( WS (WS.LobbyUpdate d), Lobby m ) ->
            ( Lobby
                { m
                    | pin = m.pin
                    , readyPlayers = d.readyPlayers
                    , totalPlayers = d.totalPlayers
                }
            , Cmd.none
            )

        ( WS (WS.LobbyDisbanded d), Lobby m ) ->
            if m.pin == d.pin then
                ( init, Cmd.none )

            else
                ( model, Cmd.none )

        ( WS (WS.StartPerformanceResponse d), Home _ ) ->
            ( Performance (Performance.init 5 d)
            , Task.perform
                (\vp ->
                    PerformanceMsg
                        (Performance.Resize (round vp.viewport.width) (round vp.viewport.height))
                )
                Browser.Dom.getViewport
            )

        ( WS (WS.StartPerformanceResponse d), Lobby m ) ->
            ( Performance (Performance.init m.instruments d)
            , Task.perform
                (\vp ->
                    PerformanceMsg
                        (Performance.Resize (round vp.viewport.width) (round vp.viewport.height))
                )
                Browser.Dom.getViewport
            )

        ---- Lobby Manager messages
        ( SettingsDraftChanged settingsDraft, Lobby m ) ->
            ( Lobby { m | manager = Just settingsDraft }, Cmd.none )

        ( StartPerformance, Lobby m ) ->
            case m.manager of
                Nothing ->
                    ( model, Cmd.none )

                Just settingsDraft ->
                    ( Lobby m, WS.send (WS.StartPerformanceRequest { pin = m.pin, settings = settingsDraft }) )

        ( LeaveLobby, Lobby m ) ->
            case m.manager of
                Nothing ->
                    ( init, WS.send (WS.LeaveLobbyRequest { pin = m.pin }) )

                Just _ ->
                    ( init, WS.send (WS.DisbandLobbyRequest { pin = m.pin }) )

        ---- Performance messages
        ( PerformanceMsg msg, Performance m ) ->
            Tuple.mapBoth
                Performance
                (Cmd.map PerformanceMsg)
                (Performance.update msg m)

        ( WS (WS.ReceiveHeartbeat { fragment }), Performance m ) ->
            Tuple.mapBoth
                Performance
                (Cmd.map PerformanceMsg)
                (Performance.update (ReceiveHeartbeat { fragment = fragment }) m)

        _ ->
            ( model, Ports.log "unknown case" )



---- Subscriptions ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ WS.receive WS -- We always want to know about WS messages
        , case model of
            Performance m ->
                Sub.map PerformanceMsg (Performance.subscriptions m)

            _ ->
                Sub.none
        ]



---- View ----


view : Element msg -> Html msg
view =
    Element.layoutWith
        { options =
            [ focusStyle
                { borderColor = Nothing
                , backgroundColor = Nothing
                , shadow = Nothing
                }
            ]
        }
        [ Background.color whitesmoke
        , Font.family [ Font.typeface "Cantarell" ]
        , Font.color grey2
        , Font.size 18
        ]


viewLayout : Model -> Element Msg
viewLayout model =
    case model of
        Home m ->
            viewHome m

        About ->
            viewAbout

        Performance m ->
            Element.map PerformanceMsg (Performance.view m)

        Tutorial ->
            viewTutorial

        Lobby m ->
            viewLobby m


btn : { bg : Color, bgHover : Color } -> List (Element.Attribute msg)
btn { bg, bgHover } =
    [ Font.color white
    , paddingEach { top = 0, bottom = 0, left = 24, right = 24 }
    , height (px 48)
    , Border.rounded 24
    , Background.color bg
    , mouseOver [ Background.color bgHover ]
    , backgroundTransition
    ]


viewHome : { pinDraft : String } -> Element Msg
viewHome m =
    el [ width fill, centerY, padding 64 ] <|
        column [ centerX, width (fill |> maximum 400), spacing 32 ]
            [ el [ Region.heading 1, centerX, Font.size 48, Font.bold ] (text "Rhythm of Life")
            , paragraph []
                [ text
                    """
                    This website provides a platform for creating a performance of Rhythm
                    of Life: a work for any number of percussion performers each with
                    between one and five untuned percussion instruments.
                    """
                ]
            , column [ width fill, spacing 16 ]
                [ row [ centerX, spacing 8 ]
                    [ Input.button (btn { bg = grey2, bgHover = grey3 })
                        { onPress = Just CreateLobbyRequest
                        , label = text "Create"
                        }
                    , row []
                        [ Input.button
                            (btn { bg = grey2, bgHover = grey3 }
                                ++ [ Border.roundEach { topLeft = 24, bottomLeft = 24, topRight = 0, bottomRight = 0 } ]
                            )
                            { onPress = Just JoinLobbyRequest
                            , label = text "Join"
                            }
                        , Input.text
                            [ width (px 96)
                            , height (px 48)
                            , centerY
                            , Border.roundEach { topLeft = 0, bottomLeft = 0, topRight = 24, bottomRight = 24 }
                            ]
                            { placeholder = Just (Input.placeholder [ centerY ] (text "Pin"))
                            , text = m.pinDraft
                            , label = Input.labelHidden ""
                            , onChange = PinDraftChanged
                            }
                        ]
                    ]
                , row [ centerX, spacing 8 ]
                    [ Input.button (btn { bg = grey2, bgHover = grey3 })
                        { onPress = Just (Navigate About), label = text "About" }
                    , Input.button (btn { bg = grey2, bgHover = grey3 })
                        { onPress = Just (Navigate Tutorial), label = text "How to use" }
                    ]
                ]
            ]


viewAbout : Element Msg
viewAbout =
    el [ centerX, centerY, padding 64 ] <|
        column [ width (fill |> maximum 640), spacing 32 ]
            [ el [ Region.heading 1, Font.size 32, Font.bold ] (text "About")
            , paragraph []
                [ text
                    """
                    This piece is inspired by the branching of the evolutionary tree of life,
                    but instead of demonstrating the evolution of genetic code, this piece
                    involves the incremental evolution of musical rhythms.
                    """
                ]
            , column [ width fill, spacing 16 ]
                [ image [ centerX ] { src = "/darwin.jpg", description = "Phylogenetic tree sketch" }
                , el [ centerX, Font.italic ] (text "Darwin's Tree of Life sketch from 1837")
                ]
            , paragraph []
                [ text
                    """
                    All performers' pathways start metronomically keeping time in unison.
                    At every new musical generation, each performer is presented with two
                    options of algorithmically generated musical rhythms which they can choose
                    or reject. The individual pathway of each player is dependent upon the
                    sequential development of their previous musical generations. Each pathway
                    develops independently, although the generative algorithm also adapts to
                    the status of the whole musical eco-system, and the mutations offered to
                    any one performer also responds to relationships between the different
                    performers' pathways.
                    """
                ]
            , paragraph []
                [ text
                    """
                    Derived from the ubiquity of pentadactyly (the five fingered-ness of most
                    animal life) all pathways maintain a constant 5-beat pattern synchronicity.
                    This relatively under-explored rhythm pattern is therefore the subject of
                    an array of rhythmic exploration.
                    """
                ]
            , paragraph []
                [ text
                    """
                    In response to the five known mass extinctions that have occurred during
                    the evolution of life on Earth, any performance can involve up to five
                    musical extinction events. When these occur each part has a high
                    probability of their pathway being made extinct and their part returning
                    to the basic metronomic pulse, from which it then develops. Those that
                    are not made extinct will continue their pathway of development. The
                    rate of mutation following an extinction event is accelerated as the
                    generative algorithm offers more changes in response to the reduction
                    of variety in the musical ecosystem.
                    """
                ]
            , paragraph []
                [ text
                    """
                    Individual parts can also go extinct at any time. This is governed by the
                    performer, who can decide to give up on the musical pathway they are on.
                    This might be in response to finding themselves in a musical dead-end, or
                    having lost synchronicity with the other performers, or having reached a
                    level of complexity that the performer cannot maintain. Once a performer
                    chooses to go extinct, they will be returned to the basic metronomic pulse.
                    """
                ]
            , paragraph []
                [ text
                    """
                    The piece does not prescribe an ending, and it is possible for a
                    performance to continue to evolve indefinitely. However, after the last
                    programmed mass extinction event, the complexity of each part will continue
                    to increase, and should the performers choose to go extinct after this time
                    then their part will not be respawned from the basic metronomic pulse.
                    """
                ]
            , el [ width fill, Border.width 1, Border.color grey9 ] Element.none
            , el [ Region.heading 3, Font.bold ] (text "Composer")
            , text "Ewan Campbell"
            , Input.button (btn { bg = grey2, bgHover = grey3 })
                { onPress = Just (Navigate init), label = text "Back" }
            ]


viewTutorial : Element Msg
viewTutorial =
    el [ centerX, centerY, padding 64 ] <|
        column [ width (fill |> maximum 640), spacing 32 ]
            [ el [ Region.heading 1, Font.size 32, Font.bold ] (text "How to use")
            , column [ spacing 16 ]
                [ el [] (text "Use the lag-o-meter ... (probably not needed)")
                , el [] (text "Choose a tempo")
                , el [] (text "Etc.")
                ]
            , Input.button (btn { bg = grey2, bgHover = grey3 })
                { onPress = Just (Navigate init), label = text "Back" }
            ]


viewLobby : LobbyModel -> Element Msg
viewLobby m =
    el [ centerX, centerY, padding 64 ] <|
        column
            [ width (fill |> maximum 640), spacing 32 ]
            [ el [ Region.heading 1, Font.size 32, Font.bold ] (text "Create concert")
            , el [ Region.heading 2, Font.size 24, Font.bold ] (text <| "Pin: " ++ (String.fromInt m.pin |> String.padLeft 6 '0'))
            , paragraph [] [ text "Choose the number of instruments that you wish to play." ]
            , el [ centerX ] (viewInstrumentSelector m.instruments)
            , column [ spacing 16 ]
                [ paragraph []
                    [ text
                        """
                        Each performer has the option to choose between 1-5
                        instruments. This will result in differing numbers of
                        staves, and may result in an increase of difficulty.
                        """
                    ]
                , paragraph []
                    [ text "Performers with different numbers of instruments can perform together." ]
                , paragraph []
                    [ text "Any untuned percussion instruments can be used." ]
                ]
            , paragraph [ centerX, Font.size 20, Font.bold ]
                [ text (String.fromInt m.readyPlayers ++ " / " ++ String.fromInt m.totalPlayers ++ " players ready") ]
            , case m.manager of
                Nothing ->
                    row [ spacing 16 ]
                        [ let
                            btnStyle =
                                if m.ready then
                                    Element.htmlAttribute (HtmlA.style "cursor" "auto")
                                        :: btn { bg = grey2 |> withAlpha 0.3, bgHover = grey2 |> withAlpha 0.3 }

                                else
                                    btn { bg = grey2, bgHover = grey3 }
                          in
                          Input.button btnStyle
                            { onPress = Just SetReady
                            , label = text "Ready"
                            }
                        , Input.button
                            (btn { bg = red |> withAlpha 0.8, bgHover = red })
                            { onPress = Just LeaveLobby, label = text "Leave" }
                        ]

                Just _ ->
                    row [ spacing 8 ]
                        [ Input.button
                            (btn { bg = grey2 |> withAlpha 0.8, bgHover = grey3 })
                            { onPress = Just StartPerformance, label = text "Start performance" }
                        , Input.button
                            (btn { bg = red |> withAlpha 0.8, bgHover = red })
                            { onPress = Just LeaveLobby, label = text "Disband" }
                        ]
            , case m.manager of
                Nothing ->
                    Element.none

                Just draft ->
                    viewManagerSettings draft
            ]


viewInstrumentSelector : Int -> Element Msg
viewInstrumentSelector instruments =
    Input.radioRow [ spacing 32 ]
        { onChange = InstrumentsChanged
        , options = [ 1, 2, 3, 4, 5 ] |> List.map (\i -> viewInstrumentOption i |> Input.optionWith i)
        , selected = Just instruments
        , label = Input.labelHidden ""
        }


viewInstrumentOption : Int -> Input.OptionState -> Element msg
viewInstrumentOption i state =
    let
        bg =
            case state of
                Input.Idle ->
                    grey9

                Input.Focused ->
                    grey9

                Input.Selected ->
                    green
    in
    el
        [ width (px 48)
        , height (px 48)
        , Border.rounded 24
        , Font.center
        , padding 14
        , Background.color (bg |> withAlpha 0.6)
        , mouseOver [ Background.color (bg |> withAlpha 0.8) ]
        , backgroundTransition
        ]
        (text (String.fromInt i))


viewManagerSettings : WS.Settings -> Element Msg
viewManagerSettings { bpm, extinction } =
    column [ width (fill |> maximum 400), spacing 32 ]
        [ el [ Region.heading 2, Font.size 24, Font.bold ] (text "Settings")
        , let
            bpmChanged bpm2 =
                SettingsDraftChanged { bpm = clamp 0 300 bpm2, extinction = extinction }
          in
          row [ width fill ]
            [ viewSlider
                { label = text "Beats per minute"
                , value = bpm
                , min = 0
                , max = 300
                , onChange = bpmChanged
                }
            , viewInput bpm bpmChanged Element.none
            ]
        , row [ width fill ]
            [ text "Number of extinction events"
            , viewInput extinction.events
                (\events -> SettingsDraftChanged { bpm = bpm, extinction = { extinction | events = clamp 0 5 events } })
                Element.none
            ]
        , let
            probabilityChanged probability =
                SettingsDraftChanged { bpm = bpm, extinction = { extinction | probability = clamp 0 100 probability } }
          in
          row [ width fill ]
            [ viewSlider
                { label = text "Probability of extinction during event"
                , value = extinction.probability
                , min = 0
                , max = 100
                , onChange = probabilityChanged
                }
            , viewInput extinction.probability
                probabilityChanged
                (el [ moveLeft 25, centerY, Font.color grey6 ] (text "%"))
            ]
        , let
            rateChanged rate =
                SettingsDraftChanged { bpm = bpm, extinction = { extinction | rate = clamp 0 100 rate } }
          in
          row [ width fill ]
            [ viewSlider
                { label = text "Rate of extinction"
                , value = extinction.rate
                , min = 0
                , max = 20
                , onChange = rateChanged
                }
            , viewInput extinction.rate
                rateChanged
                (el [ moveLeft 25, centerY, Font.color grey6 ] (text "%"))
            ]
        ]


viewSlider :
    { label : Element msg
    , value : Int
    , min : Int
    , max : Int
    , onChange : Int -> msg
    }
    -> Element msg
viewSlider { label, value, min, max, onChange } =
    Input.slider
        [ width (px 240)
        , Font.alignRight
        , Element.behindContent
            (el
                [ width fill
                , height (px 2)
                , centerY
                , Background.color grey9
                , Border.rounded 2
                ]
                Element.none
            )
        ]
        { label = Input.labelAbove [] label
        , value = toFloat value
        , min = toFloat min
        , max = toFloat max
        , step = Just 1
        , thumb =
            Input.thumb
                [ width (px 16)
                , height (px 16)
                , Border.rounded 8
                , Background.color grey5
                ]
        , onChange = round >> onChange
        }


viewInput : Int -> (Int -> msg) -> Element msg -> Element msg
viewInput value onChange onRight =
    Input.text [ width (px 72), alignRight, Element.onRight onRight ]
        { label = Input.labelHidden ""
        , placeholder = Nothing
        , text = String.fromInt value
        , onChange = String.filter Char.isDigit >> String.toInt >> Maybe.withDefault 0 >> onChange
        }



---- Main ----


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> ( init, Cmd.none )
        , update = update
        , view = viewLayout >> view
        , subscriptions = subscriptions
        }
