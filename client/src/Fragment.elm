module Fragment exposing (Fragment, Timeseries, decodeTS, encodeTS, init, mutate, toTS, view)

import Array exposing (Array)
import Element exposing (Element, height, px, width)
import Html
import Html.Attributes as HtmlA
import Json.Decode as JsonD
import Json.Encode as JsonE
import List.Extra as List
import Random
import Random.Extra as Random
import Random.List



---- Model ----


{-| A Fragment is a single bar of 5/4 music.
Invariant: Array contains 5 elements always.

  - Beat: the actual beat
  - Bool: whether this beat was last mutated

-}
type alias Fragment =
    Array ( Beat, Bool )


{-| A single beat in a fragment.
Triplets and Semiquavers rhythms that are equivalent to a single crotchet note or rest should be replaced by that.
Note that Quavers are subsumed by Semiquavers, e.g.

    Semiquavers (Note _) Rest (Note _) Rest

-}
type Beat
    = Crotchet Data
    | Triplets Data Data Data
    | Semiquavers Data Data Data Data


{-| Data for each possible place where an attack could be.
-}
type Data
    = Rest
    | Note { pitch : Int, accent : Bool }



---- Mutations ----
{- These rules are the composer's expression of musical mutation. -}


{-| A "Mutation" means: "return a list of possible next steps".
-}
type alias Mutation a =
    a -> List a


noteRange : Int -> List Int
noteRange instruments =
    List.range 0 (instruments - 1)


{-| Calculate every possible mutation of a beat, by applying f
to every possible attack, and lifting the result into a list of beats.
-}
mutateBeatBy : Mutation Data -> Mutation Beat
mutateBeatBy f beat =
    case beat of
        Crotchet d ->
            List.map Crotchet (f d)

        Triplets n1 n2 n3 ->
            {- Could be optimised as List.map, but using List.lift3 for clarity -}
            []
                ++ List.lift3 Triplets (f n1) [ n2 ] [ n3 ]
                ++ List.lift3 Triplets [ n1 ] (f n2) [ n3 ]
                ++ List.lift3 Triplets [ n1 ] [ n2 ] (f n3)

        Semiquavers n1 n2 n3 n4 ->
            []
                ++ List.lift4 Semiquavers (f n1) [ n2 ] [ n3 ] [ n4 ]
                ++ List.lift4 Semiquavers [ n1 ] (f n2) [ n3 ] [ n4 ]
                ++ List.lift4 Semiquavers [ n1 ] [ n2 ] (f n3) [ n4 ]
                ++ List.lift4 Semiquavers [ n1 ] [ n2 ] [ n3 ] (f n4)


{-| Mutate note data by changing the pitch, keeping the accent constant.
If it's a rest, return an empty list as there aren't any pitch mutations possible.
-}
mutateDataByPitch : Int -> Mutation Data
mutateDataByPitch instruments data =
    case data of
        Rest ->
            []

        Note n ->
            noteRange instruments
                |> List.remove n.pitch
                |> List.map (\pitch -> Note { pitch = pitch, accent = n.accent })


{-| Mutate note data by changing the accent, keeping pitch constant.
If it's a rest, return an empty list as there aren't any accent mutations possible.
-}
mutateDataByAccent : Mutation Data
mutateDataByAccent data =
    case data of
        Rest ->
            []

        Note n ->
            [ Note { n | accent = not n.accent } ]


{-| Mutate a beat by adding a note in a spot where there used to be a rest.
-}
mutateRhythmAddition : Int -> Mutation Beat
mutateRhythmAddition instruments beat =
    let
        default =
            List.map (\pitch -> Note { pitch = pitch, accent = False }) (noteRange instruments)

        addIfRestTo data =
            case data of
                Rest ->
                    default

                Note _ ->
                    []
    in
    case beat of
        Crotchet Rest ->
            List.map Crotchet default

        Crotchet (Note n) ->
            List.lift4 Semiquavers [ Note n ] [ Rest ] default [ Rest ]

        Triplets n1 n2 n3 ->
            []
                ++ List.lift3 Triplets (addIfRestTo n1) [ n2 ] [ n3 ]
                ++ List.lift3 Triplets [ n1 ] (addIfRestTo n2) [ n3 ]
                ++ List.lift3 Triplets [ n1 ] [ n2 ] (addIfRestTo n3)

        Semiquavers n1 n2 n3 n4 ->
            []
                ++ List.lift4 Semiquavers (addIfRestTo n1) [ n2 ] [ n3 ] [ n4 ]
                ++ List.lift4 Semiquavers [ n1 ] (addIfRestTo n2) [ n3 ] [ n4 ]
                ++ List.lift4 Semiquavers [ n1 ] [ n2 ] (addIfRestTo n3) [ n4 ]
                ++ List.lift4 Semiquavers [ n1 ] [ n2 ] [ n3 ] (addIfRestTo n4)


{-| Mutate a beat by moving an attack in the rhythm to a different, empty spot.
-}
mutateRhythmMove : Mutation Beat
mutateRhythmMove beat =
    case beat of
        -- Crotchets
        Crotchet Rest ->
            []

        Crotchet (Note n) ->
            [ Semiquavers Rest Rest (Note n) Rest ]

        -- Triplets
        Triplets Rest Rest Rest ->
            []

        Triplets (Note n1) Rest Rest ->
            [ Triplets Rest (Note n1) Rest ]

        Triplets Rest (Note n1) Rest ->
            [ Triplets (Note n1) Rest Rest
            , Triplets Rest Rest (Note n1)
            ]

        Triplets Rest Rest (Note n1) ->
            [ Triplets Rest (Note n1) Rest ]

        Triplets Rest (Note n2) (Note n3) ->
            [ Triplets (Note n2) Rest (Note n3) ]

        Triplets (Note n1) Rest (Note n3) ->
            [ Triplets Rest (Note n1) (Note n3)
            , Triplets (Note n1) (Note n3) Rest
            ]

        Triplets (Note n1) (Note n2) Rest ->
            [ Triplets (Note n1) Rest (Note n2) ]

        Triplets (Note _) (Note _) (Note _) ->
            []

        -- Semiquavers
        Semiquavers Rest Rest Rest Rest ->
            []

        Semiquavers (Note n1) Rest Rest Rest ->
            [ Semiquavers Rest (Note n1) Rest Rest ]

        Semiquavers Rest (Note n1) Rest Rest ->
            [ Semiquavers (Note n1) Rest Rest Rest
            , Semiquavers Rest Rest (Note n1) Rest
            ]

        Semiquavers Rest Rest (Note n1) Rest ->
            [ Semiquavers Rest (Note n1) Rest Rest
            , Semiquavers Rest Rest Rest (Note n1)
            ]

        Semiquavers Rest Rest Rest (Note n1) ->
            [ Semiquavers Rest Rest (Note n1) Rest ]

        Semiquavers (Note _) (Note _) (Note _) (Note _) ->
            []

        _ ->
            []


{-| Mutate a beat by converting a triplet rhythm into an approximation of a semiquaver rhythm, and vice-versa.
The converse direction might drop a note.
-}
mutateRhythmMetamorphose : Mutation Beat
mutateRhythmMetamorphose beat =
    case beat of
        Crotchet _ ->
            []

        Triplets n1 n2 n3 ->
            {- Expand a triplet into a semiquaver approximation -}
            [ Semiquavers Rest n1 n2 n3
            , Semiquavers n1 Rest n2 n3
            , Semiquavers n1 n2 Rest n3
            , Semiquavers n1 n2 n3 Rest
            ]

        Semiquavers n1 n2 n3 n4 ->
            {- Reduce semiquavers into a triplet approximation -}
            [ Triplets n2 n3 n4
            , Triplets n1 n3 n4
            , Triplets n1 n2 n4
            , Triplets n1 n2 n3
            ]


{-| Initial fragment with 5 crotchets of pitch = 0, accent = False
-}
init : Fragment
init =
    Array.repeat 5 ( Crotchet (Note { pitch = 0, accent = False }), False )


{-| Possible types of mutation.
-}
type MutationFeature
    = Pitch
    | Accent
    | Rhythm


{-| Mutate a fragment n times. Uses the Random (state) monad.

Why not use the Random monad for all the functions above?
Because we want to first generate all possibilities in a list,
and then clean it up later, by:

  - Canonicalising the data, so that Triplets (Note n) Rest Rest --> Crotchet (Note n)
  - Removing duplicates
  - Ensuring at least one change happens

-}
mutate : Int -> Int -> List Timeseries -> Fragment -> Random.Generator Fragment
mutate instruments n heartbeats fragment =
    List.foldl
        (\() -> Random.andThen2 (mutateBeatIndex instruments heartbeats) (Random.int 0 4))
        (Random.constant (clearAnnotations fragment))
        (List.repeat n ())


mutateBeatIndex : Int -> List Timeseries -> Int -> Fragment -> Random.Generator Fragment
mutateBeatIndex instruments heartbeats index fragment =
    -- Precondition: 0 <= index < 5
    let
        beat =
            -- Default should never happen.
            Array.get index fragment |> Maybe.map Tuple.first |> Maybe.withDefault (Crotchet Rest)

        canonicalise data =
            case data of
                Triplets n Rest Rest ->
                    Crotchet n

                Semiquavers n Rest Rest Rest ->
                    Crotchet n

                _ ->
                    data

        clean =
            List.map canonicalise >> List.unique >> List.remove beat

        feature =
            Random.weighted ( 2, Pitch ) [ ( 1, Accent ), ( 3, Rhythm ) ]

        choices =
            feature
                |> Random.map
                    (\feat ->
                        case feat of
                            Pitch ->
                                mutateBeatBy (mutateDataByPitch instruments) beat |> clean

                            Accent ->
                                mutateBeatBy mutateDataByAccent beat |> clean

                            Rhythm ->
                                let
                                    addition =
                                        mutateRhythmAddition instruments beat |> clean

                                    deletion =
                                        mutateBeatBy (always [ Rest ]) beat |> clean

                                    moves =
                                        mutateRhythmMove beat |> clean

                                    metamorphoses =
                                        mutateRhythmMetamorphose beat |> clean
                                in
                                addition ++ deletion ++ moves ++ metamorphoses
                    )

        heartbeatsTSB =
            heartbeats
                |> List.map (List.getAt index)
                |> List.map (Maybe.withDefault (List.repeat 12 emptyTST))

        newBeat =
            choices
                |> Random.andThen Random.List.shuffle
                |> Random.andThen
                    (\initialChoices ->
                        let
                            prunedChoices =
                                initialChoices
                                    -- Annotate each choice with an Int describing how similar it is to the heartbeats received so far.
                                    |> List.map
                                        (\b ->
                                            ( similarityTSBWithHeartbeatTSB
                                                (beatToTSB b)
                                                heartbeatsTSB
                                            , b
                                            )
                                        )
                                    -- Sort by this derived similarity score (from most dissimilar to most similar).
                                    |> List.sortBy Tuple.first
                                    -- Remove the annotation.
                                    |> List.map Tuple.second
                                    -- Take the first 8 that are least similar.
                                    |> List.take 8
                        in
                        case prunedChoices of
                            [] ->
                                Random.uniform beat []

                            h :: t ->
                                Random.uniform h t
                    )
    in
    newBeat |> Random.map (\b -> Array.set index ( b, True ) fragment)


clearAnnotations : Fragment -> Fragment
clearAnnotations =
    Array.map (Tuple.mapSecond <| always False)



---- View ----
{- We use HTML custom elements like so:
   <https://guide.elm-lang.org/interop/custom_elements>
   Everything has to be serialised into JSON, as the element can only accept string properties.
-}


{-| This is a custom element, defined in JavaScript, that renders everything using [VexFlow](https://github.com/vexflow/vexflow).
The code for that lives in main.ts.
-}
view : Fragment -> Element msg
view fragment =
    Element.el [ width (px 540), height (px 240) ] <|
        Element.html <|
            Html.node "elm-vexflow"
                [ HtmlA.attribute "data" (serialise fragment |> JsonE.encode 0) ]
                []


{-| Serialise a Fragment to be sent to VexFlow.
-}
serialise : Fragment -> JsonE.Value
serialise =
    JsonE.array (\( b, changed ) -> JsonE.object [ ( "beat", serialiseBeat b ), ( "changed", JsonE.bool changed ) ])


serialiseBeat : Beat -> JsonE.Value
serialiseBeat beat =
    case beat of
        Crotchet n ->
            JsonE.object
                [ ( "data", JsonE.list serialiseData [ ( n, "q" ) ] ), ( "triplet", JsonE.bool False ) ]

        Triplets n1 n2 n3 ->
            let
                {- Duration names in VexFlow: read as "8th", "quarter", "quarter dotted" -}
                durations =
                    [ "8", "q", "qd" ]

                {-
                   Steps:
                   - Group into [ (Note .., [Rest]), (Note .., []) ] pairs
                   - Take the number of "rests" in the second part of the pair, i.e. a note lasting two ticks:

                          (Note _, [Rest]) --> (Note, 1)

                   - The second integer is the number of ticks minus one, but we keep it like this,
                     because we want to zero-index into the list of durations.
                -}
                groups =
                    [ n1, n2, n3 ]
                        |> List.groupWhile (always <| (==) Rest)
                        |> List.map (Tuple.mapSecond List.length)
                        -- Default case should not happen.
                        |> List.map (Tuple.mapSecond (\x -> List.getAt x durations |> Maybe.withDefault "8"))
            in
            JsonE.object [ ( "data", JsonE.list serialiseData groups ), ( "triplet", JsonE.bool True ) ]

        Semiquavers n1 n2 n3 n4 ->
            let
                {- Duration names in VexFlow: read as "16th", "8th", "8th dotted", "quarter" -}
                durations =
                    [ "16", "8", "8d", "q" ]

                groups =
                    [ n1, n2, n3, n4 ]
                        |> List.groupWhile (\_ -> (==) Rest)
                        |> List.map (Tuple.mapSecond List.length)
                        -- Default case should not happen.
                        |> List.map (Tuple.mapSecond (\x -> List.getAt x durations |> Maybe.withDefault "8"))
            in
            JsonE.object [ ( "data", JsonE.list serialiseData groups ), ( "triplet", JsonE.bool False ) ]


{-| The duration of a note Data depends on the context it is in, so we accept it as an argument.
This function isn't curried because it's used like:

    JsonE.list serialiseData groups

and `groups` has type `List ( Data, String )`.

-}
serialiseData : ( Data, String ) -> JsonE.Value
serialiseData ( data, duration ) =
    let
        {- E4, G4, B4, D5, F5 are the names of the notes that sit on the lines on a treble clef stave.
           We use them to represent the (up to) five pitches that notes can be.
        -}
        pitchNames =
            [ "e/4", "g/4", "b/4", "d/5", "f/5" ]
    in
    case data of
        Rest ->
            JsonE.object
                [ ( "type", JsonE.string "rest" )
                , ( "duration", JsonE.string duration )
                ]

        Note n ->
            JsonE.object
                [ ( "type", JsonE.string "note" )
                , ( "duration", JsonE.string duration )

                -- Default case should not happen.
                , ( "pitch", JsonE.string (List.getAt n.pitch pitchNames |> Maybe.withDefault "b/4") )
                , ( "accent", JsonE.bool n.accent )
                ]



---- Time series encoding ----
{-
   For exchanging fragment data in an easily comparable format.
-}


type alias Timeseries =
    List TimeseriesBeat


type alias TimeseriesBeat =
    List TimeseriesTick


type alias TimeseriesTick =
    { attack : Bool
    , pitch : Int
    , accent : Bool
    }


toTS : Fragment -> Timeseries
toTS =
    Array.toList >> List.map (Tuple.first >> beatToTSB)


emptyTST : TimeseriesTick
emptyTST =
    { attack = False, pitch = 0, accent = False }


beatToTSB : Beat -> TimeseriesBeat
beatToTSB beat =
    case beat of
        Crotchet data ->
            dataToTST data :: List.repeat 11 emptyTST

        Triplets data1 data2 data3 ->
            []
                -- Length 12
                ++ (dataToTST data1 :: List.repeat 3 emptyTST)
                ++ (dataToTST data2 :: List.repeat 3 emptyTST)
                ++ (dataToTST data3 :: List.repeat 3 emptyTST)

        Semiquavers data1 data2 data3 data4 ->
            []
                -- Length 12
                ++ (dataToTST data1 :: List.repeat 2 emptyTST)
                ++ (dataToTST data2 :: List.repeat 2 emptyTST)
                ++ (dataToTST data3 :: List.repeat 2 emptyTST)
                ++ (dataToTST data4 :: List.repeat 2 emptyTST)


dataToTST : Data -> TimeseriesTick
dataToTST data =
    case data of
        Rest ->
            emptyTST

        Note n ->
            { attack = True, pitch = n.pitch, accent = n.accent }


similarityTSBWithHeartbeatTSB : TimeseriesBeat -> List TimeseriesBeat -> Int
similarityTSBWithHeartbeatTSB tsb h =
    List.map (similarityTSB tsb) h |> List.sum


similarityTSB : TimeseriesBeat -> TimeseriesBeat -> Int
similarityTSB tsb1 tsb2 =
    List.map2
        (\tst1 tst2 ->
            0
                + 3
                * boolToInt (tst1.attack == tst2.attack)
                + 2
                * boolToInt (tst1.pitch == tst2.pitch)
                + 1
                * boolToInt (tst1.accent == tst2.accent)
        )
        tsb1
        tsb2
        |> List.sum


boolToInt : Bool -> Int
boolToInt b =
    if b then
        1

    else
        0


intToBool : Int -> Bool
intToBool b =
    b > 0



---- JSON Encoders ----


encodeTS : Timeseries -> JsonE.Value
encodeTS =
    JsonE.list (JsonE.list encodeTST)


encodeTST : TimeseriesTick -> JsonE.Value
encodeTST { attack, pitch, accent } =
    JsonE.list identity
        [ JsonE.int (boolToInt attack)
        , JsonE.int pitch
        , JsonE.int (boolToInt accent)
        ]



---- JSON Decoders ----


decodeTS : JsonD.Decoder Timeseries
decodeTS =
    JsonD.list (JsonD.list decodeTSB)


decodeTSB : JsonD.Decoder TimeseriesTick
decodeTSB =
    JsonD.map3 TimeseriesTick
        (JsonD.map intToBool <| JsonD.index 0 JsonD.int)
        (JsonD.index 1 JsonD.int)
        (JsonD.map intToBool <| JsonD.index 2 JsonD.int)
