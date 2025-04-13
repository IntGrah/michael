module Performance exposing (Choice(..), Model, Msg(..), init, subscriptions, update, view)

import Browser.Events
import Element exposing (Color, Element, alpha, centerX, centerY, column, el, height, image, inFront, moveDown, moveLeft, moveRight, moveUp, onRight, padding, px, rotate, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Input as Input
import Fragment exposing (Fragment)
import Html.Attributes as HtmlA
import Json.Decode as JsonD
import Ports
import Random
import Random.Extra as Random
import Theme exposing (backgroundTransition, grey5, grey9, white, withAlpha)
import Time
import Vector exposing (Vector)
import WebSocket



---- Model ----


type alias Model =
    { -- Properties of the performance
      pin : Int
    , bpm : Int
    , programme : List { generation : Int, extinct : Bool }
    , start : Time.Posix
    , seed : Random.Seed
    , mutationRate : Int -- Increased after mass extinction
    , heartbeatsReceived : List Fragment.Timeseries

    -- User settings
    , instruments : Int
    , randomChoiceEnabled : Bool
    , soundEnabled : Bool
    , chosen : Bool
    , window : ( Int, Int )

    -- Timekeeping
    , monotonicTime : Float
    , monotonicBeat : Int

    -- Fragments
    , archive : List ( Vector, Fragment )
    , fragments : Twig
    , incoming : Twig
    , choice : Choice
    }


type alias Twig =
    { current : Fragment
    , left : Fragment
    , right : Fragment
    }


{-| If it is before the performance start time, return nothing. Otherwise, return
Just i, where i is a non-negative float, equal to

    elapsedTime / amountOfTimePerBeat.

The quotient and remainder of floor i / 5 is the current bar number, and the beat within the bar.

-}
monotonicTimeOf : Model -> Time.Posix -> Float
monotonicTimeOf { start, bpm } now =
    let
        ms =
            Time.posixToMillis now - Time.posixToMillis start
    in
    toFloat (ms * bpm) / (1000 * 60)



---- Init ----


init : Int -> WebSocket.PerformanceData -> Model
init instruments data =
    let
        ( ( left, right ), seed2 ) =
            Random.step
                (Random.pair
                    (Fragment.mutate instruments 1 [] Fragment.init)
                    (Fragment.mutate instruments 1 [] Fragment.init)
                )
                (Random.initialSeed data.seed)
    in
    { -- Properties of the performance
      pin = data.pin
    , bpm = data.bpm
    , programme = data.programme
    , start = Time.millisToPosix data.start
    , seed = seed2
    , mutationRate = 1
    , heartbeatsReceived = []

    -- User settings
    , instruments = instruments
    , randomChoiceEnabled = False
    , soundEnabled = False
    , chosen = False
    , window = ( 0, 0 )

    -- Timekeeping
    , monotonicTime = 0.0
    , monotonicBeat = 0 -- Dummy initial value; will be set by Tick message

    -- Fragments
    , archive = []
    , fragments =
        { current = Fragment.init
        , left = left
        , right = right
        }
    , incoming =
        -- Dummy initial value
        { current = Fragment.init
        , left = Fragment.init
        , right = Fragment.init
        }
    , choice = NoOp
    }


type Choice
    = NoOp
    | Left
    | Right
    | Extinction


{-| Whether the current generation is an extinction event.
-}
programmedExtinction : Model -> Maybe Bool
programmedExtinction model =
    case
        List.filter (\a -> a.generation == (model.monotonicBeat // 20)) model.programme
    of
        [] ->
            Nothing

        h :: _ ->
            Just h.extinct



---- Update ----


type Msg
    = Tick Time.Posix
    | ChoiceChanged Choice
    | Resize Int Int
    | SoundEnabledToggled
    | RandomChoiceEnabledToggled
    | AnimationFrame Time.Posix
    | SendHeartbeat
    | ReceiveHeartbeat { fragment : Fragment.Timeseries }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationFrame _ ->
            ( model, Cmd.none )

        Tick time ->
            {- Poll the Elm runtime for the time, and perform actions based on it. -}
            let
                monotonicTime2 =
                    monotonicTimeOf model time

                monotonicBeat2 =
                    floor monotonicTime2

                model2 =
                    { model | monotonicTime = monotonicTime2, monotonicBeat = monotonicBeat2 }
            in
            if model.monotonicBeat == monotonicBeat2 || monotonicBeat2 < 0 then
                -- Not a new beat,
                ( model2, Cmd.none )

            else
                let
                    cmd =
                        if not model.soundEnabled then
                            Cmd.none

                        else if (monotonicBeat2 |> modBy 5) == 0 then
                            -- A5
                            Ports.beep 880

                        else
                            -- A4
                            Ports.beep 440
                in
                ( handleNewBeat model2, cmd )

        Resize w h ->
            ( { model | window = ( w, h ) }, Cmd.none )

        ChoiceChanged choice ->
            if 15 <= (model.monotonicBeat |> modBy 20) then
                -- Locked in, cannot change
                ( model, Cmd.none )

            else
                ( { model | choice = choice, chosen = True }, Cmd.none )

        SoundEnabledToggled ->
            ( { model | soundEnabled = not model.soundEnabled }, Cmd.none )

        RandomChoiceEnabledToggled ->
            ( { model | randomChoiceEnabled = not model.randomChoiceEnabled }, Cmd.none )

        SendHeartbeat ->
            ( model
            , WebSocket.send
                (WebSocket.SendHeartbeat
                    { pin = model.pin
                    , fragment = Fragment.toTS model.fragments.current
                    }
                )
            )

        ReceiveHeartbeat { fragment } ->
            ( { model
                | heartbeatsReceived = fragment :: model.heartbeatsReceived |> List.take 10
              }
            , Cmd.none
            )


handleNewBeat : Model -> Model
handleNewBeat model =
    if (model.monotonicBeat |> modBy 20) == 15 then
        -- Last bar of generation; locked in choice
        handleLockIn model

    else if (model.monotonicBeat |> modBy 20) == 0 then
        -- First bar of generation
        handleNewGeneration model

    else
        -- Just a new beat, nothing special
        model


{-| On the last bar of a generation, calculate the next set of options.
-}
handleLockIn : Model -> Model
handleLockIn model =
    let
        isExtinction =
            programmedExtinction model |> Maybe.withDefault False

        ( choice2, seed2 ) =
            if isExtinction then
                ( Extinction, model.seed )

            else if model.chosen then
                ( model.choice, model.seed )

            else if model.randomChoiceEnabled then
                Random.step (Random.choice Left Right) model.seed

            else
                ( NoOp, model.seed )

        incomingCurrent =
            case choice2 of
                Left ->
                    model.fragments.left

                Right ->
                    model.fragments.right

                NoOp ->
                    model.fragments.current

                Extinction ->
                    Fragment.init

        ( ( incomingLeft, incomingRight ), seed3 ) =
            Random.step
                (Random.pair
                    (Fragment.mutate model.instruments model.mutationRate model.heartbeatsReceived incomingCurrent)
                    (Fragment.mutate model.instruments model.mutationRate model.heartbeatsReceived incomingCurrent)
                )
                seed2
    in
    { model
        | incoming =
            { current = incomingCurrent
            , left = incomingLeft
            , right = incomingRight
            }
        , mutationRate =
            if isExtinction then
                3

            else
                model.mutationRate - 1 |> max 1
        , choice = choice2
        , seed = seed3
    }


{-| On the beginning of a new generation, swap in the incoming options, and bump the camera.
-}
handleNewGeneration : Model -> Model
handleNewGeneration model =
    { model
        | fragments = model.incoming
        , choice = NoOp
        , chosen = False
    }



---- Subscriptions ----


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Time.every 10 Tick
        , Browser.Events.onAnimationFrame AnimationFrame
        , Browser.Events.onKeyDown keyDecoder
        , Time.every 5000 (\_ -> SendHeartbeat)
        , Browser.Events.onResize Resize
        ]


keyDecoder : JsonD.Decoder Msg
keyDecoder =
    JsonD.field "key" JsonD.string
        |> JsonD.andThen
            (\str ->
                case str of
                    "ArrowLeft" ->
                        JsonD.succeed (ChoiceChanged Left)

                    "ArrowRight" ->
                        JsonD.succeed (ChoiceChanged Right)

                    "ArrowUp" ->
                        JsonD.succeed (ChoiceChanged NoOp)

                    "ArrowDown" ->
                        JsonD.succeed (ChoiceChanged NoOp)

                    _ ->
                        JsonD.fail "Not an arrow"
            )



---- View ----


btn : Color -> List (Element.Attribute msg)
btn color =
    [ padding 8
    , width (px 48)
    , height (px 48)
    , Border.width 0
    , Border.rounded 24
    , backgroundTransition
    , Background.color (color |> withAlpha 0.6)
    , Element.mouseOver [ Background.color (color |> withAlpha 0.8) ]
    ]


view : Model -> Element Msg
view model =
    el [ centerX, padding 16 ] <|
        column []
            [ el [ centerX ] <|
                row [ centerY, padding 32, spacing 16 ]
                    [ viewMetronome model.monotonicBeat
                    , viewBarCounter model.monotonicBeat
                    , Input.button
                        (btn
                            (if model.soundEnabled then
                                Theme.green

                             else
                                Theme.grey9
                            )
                        )
                        { onPress = Just SoundEnabledToggled
                        , label =
                            image []
                                { src =
                                    if model.soundEnabled then
                                        "/volume_on.svg"

                                    else
                                        "/volume_off.svg"
                                , description = "Toggle sound"
                                }
                        }
                    , Input.button
                        (btn
                            (if model.randomChoiceEnabled then
                                Theme.green

                             else
                                Theme.grey9
                            )
                        )
                        { onPress = Just RandomChoiceEnabledToggled
                        , label =
                            image []
                                { src = "/dice.svg"
                                , description = "Toggle random choice"
                                }
                        }
                    , Input.button
                        (btn
                            (if programmedExtinction model /= Nothing then
                                Theme.red

                             else if model.choice == Extinction then
                                Theme.green

                             else
                                Theme.grey9
                            )
                        )
                        { onPress = Just (ChoiceChanged Extinction)
                        , label =
                            image []
                                { src = "/skull.svg"
                                , description = "Extinction"
                                }
                        }
                    ]
            , if model.window == ( 0, 0 ) then
                Element.none

              else if Tuple.first model.window < 900 then
                el
                    [ width (px 640)
                    , Element.scale (4 / 9)
                    , inFront <| el [ moveLeft 400 ] (viewFragments model)
                    ]
                    Element.none

              else if Tuple.first model.window < 1200 then
                el
                    [ width (px 800)
                    , Element.scale (5 / 9)
                    , inFront <| el [ moveLeft 320 ] (viewFragments model)
                    ]
                    Element.none

              else if Tuple.first model.window < 1500 then
                el
                    [ width (px 1120)
                    , Element.scale (7 / 9)
                    , inFront <| el [ moveLeft 160 ] (viewFragments model)
                    ]
                    Element.none

              else
                el
                    [ width (px 1440)
                    , inFront (viewFragments model)
                    ]
                    Element.none
            ]


viewMetronome : Int -> Element msg
viewMetronome beat =
    row [ spacing 4 ]
        (List.range 0 4
            |> List.map
                (\n ->
                    let
                        color =
                            if n <= modBy 5 beat && beat >= 0 then
                                -- Highlight if <= beat
                                Theme.grey5

                            else
                                Theme.grey9
                    in
                    el
                        [ Background.color color
                        , width (px 32)
                        , height (px 32)
                        , Border.rounded 16
                        , Theme.backgroundTransition
                        ]
                        Element.none
                )
        )


viewBarCounter : Int -> Element msg
viewBarCounter beat =
    el
        [ width (px 160)
        , padding 8
        , Border.width 2
        , Border.color Theme.grey9
        , Border.rounded 8
        ]
        (if beat < 0 then
            text "Starting..."

         else
            text
                (""
                    ++ (String.fromInt <| beat // 20 + 1)
                    ++ ": bar "
                    ++ (String.fromInt <| modBy 4 (beat // 5) + 1)
                    ++ ", beat "
                    ++ (String.fromInt <| modBy 5 beat + 1)
                )
        )


viewFragments : Model -> Element Msg
viewFragments model =
    let
        {- Progress from the start of bar 4 to the bar 1 of the next generation.
           Invariant: 0.0 <= progress < 1.0.
           e.g. if monotonicTime = 56.5, monotonicBeat = 56, then
           56 // 20 * 20 = 40 (greatest multiple of 20 that is <= 56)
           56.5 - 40 - 15 = 1.5
           1.5 / 5 = 0.3
           So we are 30% of the way there.
        -}
        progress =
            (model.monotonicTime - toFloat (model.monotonicBeat // 20 * 20) - 15) / 5 |> clamp 0 1

        {- Apply sinusoidal easing with delay.
           Invariant: 0.0 <= easedProgress < 1.0
        -}
        early =
            (1 - cos (pi * clamp 0 1 ((progress - 0.2) / 0.4))) / 2

        late =
            (1 - cos (pi * clamp 0 1 ((progress - 0.6) / 0.4))) / 2

        {- Where to position the current fragment, animated. -}
        cameraVector : Vector
        cameraVector =
            case model.choice of
                NoOp ->
                    ( 0, 0 )

                Left ->
                    ( 720, -216 )

                Right ->
                    ( 720, 216 )

                Extinction ->
                    ( 0, 0 )

        {- Position of the current fragment. -}
        ( sx, sy ) =
            Vector.sub ( 90, 280 ) (Vector.scale late cameraVector)

        {- Change in +x, +/-y relative to the current fragment. -}
        ( dx, dy ) =
            ( 720, 216 )

        borderGreen =
            Border.color Theme.green

        borderNone =
            Border.color (Theme.white |> withAlpha 0)

        borderGreenIf choice =
            if choice == model.choice then
                borderGreen

            else
                borderNone

        fadeOut timing =
            Element.alpha (1 - timing)

        fadeIn timing =
            Element.alpha timing

        dots =
            modBy 20 model.monotonicBeat // 5 + 1

        arrow =
            image
                [ moveRight 50, height (px 40), alpha 0.6 ]
                { src = "/public/arrow.svg"
                , description = "Arrow"
                }

        arrowLeft =
            el [ moveUp 8, rotate -0.4 ] arrow

        arrowRight =
            el [ moveDown 208, rotate 0.4 ] arrow
    in
    el
        [ width (px 1440)
        , height (px 800)
        , Border.width 2
        , Border.color (grey9 |> withAlpha 0.8)
        , Border.rounded 16
        , Element.clip
        , Element.htmlAttribute (HtmlA.attribute "style" "user-select: none") -- apparently elm-ui doesn't have this

        -- , viewAll (List.map (\( ( x, y ), frag ) -> viewFragmentBox [ moveRight x, moveDown y ] (Fragment.view frag)) model.archive)
        , inFront <|
            if (model.monotonicBeat |> modBy 20) < 15 || model.monotonicBeat < 0 then
                -- Normal mode: first three bars out of four
                viewFragmentBox
                    [ moveRight sx
                    , moveDown sy
                    , inFront (viewDots dots)
                    , borderGreenIf NoOp

                    -- Left
                    , onRight arrowLeft
                    , inFront <|
                        viewFragmentBox
                            [ moveRight dx
                            , moveUp dy
                            , Element.pointer
                            , Events.onClick (ChoiceChanged Left)
                            , inFront (viewDots 0)
                            , borderGreenIf Left
                            ]
                            (Fragment.view model.fragments.left)

                    -- Right
                    , onRight arrowRight
                    , inFront <|
                        viewFragmentBox
                            [ moveRight dx
                            , moveDown dy
                            , Element.pointer
                            , Events.onClick (ChoiceChanged Right)
                            , inFront (viewDots 0)
                            , borderGreenIf Right
                            ]
                            (Fragment.view model.fragments.right)
                    ]
                    (Fragment.view model.fragments.current)

            else
                let
                    {- Can attach to any element -}
                    attachIncomingChoices =
                        viewAll
                            [ viewFragmentBox
                                [ moveRight dx
                                , moveUp dy
                                , inFront (viewDots 0)
                                , borderNone
                                ]
                                (Fragment.view model.incoming.left)
                            , viewFragmentBox
                                [ moveRight dx
                                , moveDown dy
                                , inFront (viewDots 0)
                                , borderNone
                                ]
                                (Fragment.view model.incoming.right)
                            ]
                in
                -- Locked in: last bar out of four; start animating
                case model.choice of
                    Left ->
                        {-
                           current
                               left
                                   incoming left
                                   incoming right
                               right (fade out)
                        -}
                        viewFragmentBox
                            [ moveRight sx
                            , moveDown sy
                            , inFront (viewDots dots)
                            , borderNone

                            -- Left
                            , onRight <| el [ fadeOut late ] arrowLeft
                            , inFront <|
                                viewFragmentBox
                                    [ moveRight dx
                                    , moveUp dy
                                    , inFront (viewDots 0)
                                    , borderGreen

                                    -- Incoming
                                    , onRight <| el [ fadeIn early ] arrowLeft
                                    , onRight <| el [ fadeIn early ] arrowRight
                                    , inFront attachIncomingChoices
                                    ]
                                    (Fragment.view model.fragments.left)

                            -- Right
                            , onRight <| el [ fadeOut early ] arrowRight
                            , inFront <|
                                viewFragmentBox
                                    [ moveRight dx, moveDown dy, inFront (viewDots 0), borderNone, fadeOut early ]
                                    (Fragment.view model.fragments.right)
                            ]
                            (Fragment.view model.fragments.current)

                    Right ->
                        {-
                           current
                               left (fade out)
                               right
                                   incoming left
                                   incoming right
                        -}
                        viewFragmentBox
                            [ moveRight sx
                            , moveDown sy
                            , inFront (viewDots dots)
                            , borderNone

                            -- Left
                            , onRight <| el [ fadeOut early ] arrowLeft
                            , inFront <|
                                viewFragmentBox
                                    [ moveRight dx, moveUp dy, inFront (viewDots 0), borderNone, fadeOut early ]
                                    (Fragment.view model.fragments.left)

                            -- Right
                            , onRight <| el [ fadeOut late ] arrowRight
                            , inFront <|
                                viewFragmentBox
                                    [ moveRight dx
                                    , moveDown dy
                                    , inFront (viewDots 0)
                                    , borderGreen

                                    -- Incoming
                                    , onRight <| el [ fadeIn early ] arrowLeft
                                    , onRight <| el [ fadeIn early ] arrowRight
                                    , inFront attachIncomingChoices
                                    ]
                                    (Fragment.view model.fragments.right)
                            ]
                            (Fragment.view model.fragments.current)

                    NoOp ->
                        {-
                           current (fade out)
                               left (fade out)
                               right (fade out)
                           incoming current (fade in)
                               incoming left (fade in)
                               incoming right (fade in)
                        -}
                        viewFragmentBox
                            [ moveRight sx
                            , moveDown sy
                            , inFront (viewDots dots)
                            , borderGreen

                            -- Left
                            , onRight arrowLeft
                            , inFront <|
                                viewFragmentBox
                                    [ moveRight dx, moveUp dy, inFront (viewDots 0), borderNone, fadeOut early ]
                                    (Fragment.view model.fragments.left)

                            -- Right
                            , onRight arrowRight
                            , inFront <|
                                viewFragmentBox
                                    [ moveRight dx, moveDown dy, inFront (viewDots 0), borderNone, fadeOut early ]
                                    (Fragment.view model.fragments.right)

                            -- Incoming
                            , inFront <| el [ fadeIn late ] attachIncomingChoices
                            ]
                            (Fragment.view model.fragments.current)

                    Extinction ->
                        {-
                           current (fade out)
                               left (fade out)
                               right (fade out)
                           incoming current (fade in)
                               incoming left (fade in)
                               incoming right (fade in)
                        -}
                        viewAll
                            [ viewFragmentBox
                                [ moveRight sx
                                , moveDown sy
                                , inFront (viewDots dots)
                                , borderNone
                                , fadeOut early

                                -- Left
                                , onRight arrowLeft
                                , inFront <|
                                    viewFragmentBox
                                        [ moveRight dx, moveUp dy, inFront (viewDots 0), borderNone ]
                                        (Fragment.view model.fragments.left)

                                -- Right
                                , onRight arrowRight
                                , inFront <|
                                    viewFragmentBox
                                        [ moveRight dx, moveDown dy, inFront (viewDots 0), borderNone ]
                                        (Fragment.view model.fragments.right)
                                ]
                                (Fragment.view model.fragments.current)

                            -- Incoming
                            , viewFragmentBox
                                [ moveRight sx
                                , moveDown sy
                                , inFront (viewDots 0)
                                , borderNone
                                , fadeIn late
                                , inFront attachIncomingChoices
                                ]
                                (Fragment.view model.incoming.current)
                            ]
        ]
        Element.none


viewAll : List (Element msg) -> Element msg
viewAll els =
    el (List.map inFront els) Element.none


viewFragmentBox : List (Element.Attribute msg) -> Element msg -> Element msg
viewFragmentBox attributes =
    el
        ([ Background.color white
         , Border.rounded 16
         , Border.width 2
         , Border.shadow { offset = ( 0, 0 ), size = 4, blur = 8, color = grey9 |> withAlpha 0.5 }

         -- , CssT.transition [ CssT.borderColor 100 ]
         ]
            ++ attributes
        )


{-| Dots which indicate the bar number of the current generation.
-}
viewDots : Int -> Element msg
viewDots n =
    row
        [ padding 12
        , spacing 4
        ]
        (List.range 0 3
            |> List.map
                (\i ->
                    el
                        [ width (px 12)
                        , height (px 12)
                        , Border.rounded 6
                        , Border.width 2
                        , Border.color grey5
                        , Background.color
                            (if i < n then
                                Theme.grey5

                             else
                                Theme.white
                            )
                        , Theme.backgroundTransition
                        ]
                        Element.none
                )
        )
