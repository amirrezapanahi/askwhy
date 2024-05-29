port module Main exposing (Model, Msg(..), initialModel, main)

import Accessibility.Aria as Aria
import Browser
import Html
    exposing
        ( Html
        , button
        , div
        , input
        , main_
        , pre
        , span
        , text
        )
import Html.Attributes as Attr
import Html.Events exposing (onClick, onFocus, onInput)
import List.Extra exposing (removeAt)
import Maybe exposing (map, withDefault)



-- CONSTANTS


ask_why_description : String
ask_why_description =
    "By asking a series of questions and reaching primitive principles, you can determine if whatever you are putting forward time and energy towards is a worthwhile investment."



-- MESSAGES


type Msg
    = CreateThread
    | DeleteThread Int
    | MakePrinciple Int Ponder
    | RevokePrinciple Int Ponder
    | DeletePrinciple Ponder
    | GiveReason Int String
    | CreateNewSequence Thread
    | MakeSelectedThread Thread
    | MakeSelectedNode Int



-- MODEL


type alias Model =
    { threads : List Thread
    , currentNodeIdx : Int
    , currentThread : Maybe Thread
    , principles : List Ponder
    }


type alias Thread =
    { id : Int
    , name : String
    , content : ThreadContent
    }


type alias ThreadContent =
    List (Maybe Sequence)


type alias Sequence =
    ( Ponder, Maybe String )


type Why
    = String


type Ponder
    = Principle String
    | Reason String
    | Empty



-- type Node
--     = Node
--         { value : Ponder
--         , next : Maybe Node
--         }
-- INIT


initialModel : ( Model, Cmd Msg )
initialModel =
    ( { threads = []
      , currentNodeIdx = 0
      , currentThread = Nothing
      , principles = []
      }
    , Cmd.none
    )



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- VIEW


view : Model -> Html Msg
view model =
    main_
        [ Aria.label "main content"
        , Attr.class "flex flex-col justify-center align-center mx-auto items-center h-full font-serif text-center w-4/5"
        ]
        [ span
            [ Attr.class "font-semibold italic" ]
            [ text "ask why" ]
        , pre
            [ Attr.class "text-balance" ]
            [ text ask_why_description ]
        , div [ Attr.class "flex flex-row w-full" ]
            [ div [ Attr.class "h-full bg-red-200 w-1/5 " ]
                [ div
                    [ Attr.class "flex flex-row justify-around" ]
                    [ span
                        []
                        [ text "Threads" ]
                    , button
                        [ onClick CreateThread ]
                        [ text "+" ]
                    ]
                , threadListView model
                ]
            , div [ Attr.class ("h-full w-3/5 " ++ threadSpacingStyle) ]
                [ span [ Attr.class ("bg-green-200 " ++ titleStyles) ] [ text "Current Thread" ]
                , currentThreadView model.currentThread
                ]
            , div [ Attr.class "h-full bg-blue-100 w-1/5" ]
                [ span [ Attr.class titleStyles ] [ text "Principles" ]
                , principleListView model
                ]
            ]
        ]


currentThreadView : Maybe Thread -> Html Msg
currentThreadView maybeThread =
    case maybeThread of
        Just thread ->
            div [ Attr.class threadSpacingStyle ]
                [ div [ Attr.class "grid grid-cols-[1fr_auto_1fr]" ]
                    ([ span
                        [ Attr.class "text-right italic" ]
                        [ text "Start here:" ]
                     , span [] [ text "" ]
                     ]
                        ++ threadContentView thread
                    )
                ]

        Nothing ->
            text ""


renderPonderAndWhy :
    Int
    -> Int
    -> Maybe ( Ponder, Maybe String )
    -> ( Maybe Ponder, Html Msg, Html Msg ) -- (Ponder output, Why output)
renderPonderAndWhy index _ maybeSequence =
    case maybeSequence of
        Just ( ponder, why ) ->
            case ponder of
                Reason reason_text ->
                    ( Just ponder
                    , div [ Attr.class "grid grid-cols-[1fr_auto] bg-gray-100 rounded focus:outline-none focus:ring-0 border-0 p-0 m-0" ]
                        [ input
                            [ Attr.value reason_text
                            , Attr.class "bg-gray-100 rounded focus:outline-none focus:ring-0 border-0 px-2 py-0 m-0"
                            , onInput (\value -> GiveReason index value)
                            , onFocus (MakeSelectedNode index)
                            ]
                            []
                        , div
                            [ Attr.class "has-tooltip" ]
                            [ span [ Attr.class "tooltip rounded shadow-lg p-1 bg-gray-100 text-black-500 -mt-8" ] [ text "Make Principle" ]
                            , button [ onClick (MakePrinciple index ponder) ] [ text "✦" ]
                            ]
                        ]
                    , case why of
                        Just x ->
                            renderWhy x

                        Nothing ->
                            text ""
                    )

                Empty ->
                    ( Just ponder
                    , div [ Attr.class "grid grid-cols-[1fr_auto] bg-gray-100 rounded focus:outline-none focus:ring-0 border-0 p-0 m-0" ]
                        [ input
                            [ Attr.value ""
                            , Attr.class "bg-gray-100 rounded focus:outline-none focus:ring-0 border-0 px-2 py-0 m-0"
                            , onInput (\value -> GiveReason index value)
                            , onFocus (MakeSelectedNode index)
                            ]
                            []
                        ]
                    , text ""
                    )

                Principle principle_text ->
                    ( Just ponder
                    , div [ Attr.class "grid grid-cols-[1fr_auto] bg-gray-100 rounded focus:outline-none focus:ring-0 border-0 p-0 m-0" ]
                        [ input
                            [ Attr.value principle_text
                            , Attr.class "bg-blue-100 rounded focus:outline-none focus:ring-0 border-0 px-2 py-0 m-0"
                            , onInput (\value -> GiveReason index value)
                            , onFocus (MakeSelectedNode index)
                            ]
                            []
                        , div
                            [ Attr.class "has-tooltip" ]
                            [ span [ Attr.class "tooltip rounded shadow-lg p-1 bg-gray-100 text-black-500 -mt-8" ] [ text "Revoke Principle" ]
                            , button [ onClick (RevokePrinciple index ponder) ] [ text "↩" ]
                            ]
                        ]
                    , text ""
                    )

        Nothing ->
            ( Nothing, text "", text "" )


threadContentView : Thread -> List (Html Msg)
threadContentView thread =
    let
        renderSequence : Maybe Ponder -> Html Msg -> Html Msg -> List (Html Msg)
        renderSequence maybePonder ponderHtml whyHtml =
            ponderHtml
                :: whyHtml
                :: [ div
                        [ Attr.class "has-tooltip" ]
                        [ span [ Attr.class "tooltip rounded shadow-lg p-1 bg-gray-100 text-black-500 -mt-8" ] [ text "Give Reason" ]
                        , span
                            [ Attr.class "opacity-50 hover:opacity-100 cursor-pointer", onClick (CreateNewSequence thread) ]
                            [ case maybePonder of
                                Just ponder ->
                                    case ponder of
                                        Principle _ ->
                                            text ""

                                        _ ->
                                            text " → "

                                Nothing ->
                                    text ""
                            ]
                        ]
                   ]
    in
    List.indexedMap
        (\index item ->
            let
                ( ponder, ponderHtml, whyHtml ) =
                    renderPonderAndWhy index (List.length thread.content) item
            in
            renderSequence ponder ponderHtml whyHtml
        )
        thread.content
        |> flattenList


threadListView : Model -> Html Msg
threadListView model =
    div []
        (List.map
            (\thread ->
                threadView thread
                    (case model.currentThread of
                        Just current_thread ->
                            current_thread.id == thread.id

                        Nothing ->
                            False
                    )
            )
            model.threads
        )


threadView : Thread -> Bool -> Html Msg
threadView thread isCurrent =
    div [ Attr.class "flex flex-row justify-around" ]
        [ span
            [ Attr.class
                (if isCurrent then
                    "bg-slate-100 rounded"

                 else
                    ""
                )
            , onClick (MakeSelectedThread thread)
            ]
            [ text thread.name ]
        , button [ onClick (DeleteThread thread.id) ] [ text "×" ]
        ]


principleListView : Model -> Html Msg
principleListView model =
    div [] (List.map principleView model.principles)


principleView : Ponder -> Html Msg
principleView ponder =
    case ponder of
        Principle principle ->
            span
                [ Attr.class "flex flex-row justify-around " ]
                [ text principle
                , button [ onClick (DeletePrinciple ponder) ] [ text "×" ]
                ]

        _ ->
            text ""


renderWhy : String -> Html Msg
renderWhy why_text =
    div [ Attr.class "flex font-bold" ]
        [ span [] [ text "Why" ]
        , span
            [ Attr.class "italic font-light" ]
            [ text why_text ]
        , span [] [ text "?" ]
        ]


titleStyles : String
titleStyles =
    "inline-block w-full"



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateThread ->
            --create an 'untitled thread'
            let
                newThread =
                    let
                        latestThread : Maybe Thread
                        latestThread =
                            last model.threads

                        latestThreadId : Int
                        latestThreadId =
                            latestThread
                                |> Maybe.map (\thread -> thread.id)
                                |> Maybe.withDefault 0

                        -- Default ID if no thread is found
                    in
                    Thread (latestThreadId + 1)
                        "Untitled"
                        [ Just ( Reason "", Just "Untitled" ) ]

                -- (Node { value = Why "Untitled", next = Nothing })
            in
            ( { model | currentThread = Just newThread, threads = newThread :: model.threads }, Cmd.none )

        DeleteThread threadToDeleteId ->
            let
                currentThreadBeingDeleted : Int -> Bool
                currentThreadBeingDeleted id =
                    case model.currentThread of
                        Just thread ->
                            if id == thread.id then
                                True

                            else
                                False

                        Nothing ->
                            False

                removeThreadFromList : List Thread
                removeThreadFromList =
                    List.filter
                        (\thread ->
                            thread.id /= threadToDeleteId
                        )
                        model.threads
            in
            if currentThreadBeingDeleted threadToDeleteId then
                ( { model
                    | currentThread = Nothing
                    , threads = removeThreadFromList
                  }
                , Cmd.none
                )

            else
                ( { model | threads = removeThreadFromList }, Cmd.none )

        MakePrinciple index ponder ->
            -- make sequence (Principle text, Just text)
            let
                newSequence : Sequence
                newSequence =
                    case ponder of
                        Reason text ->
                            ( Principle text, Just text )

                        Principle text ->
                            ( ponder, Just text )

                        Empty ->
                            ( ponder, Just "" )

                -- update threadcontent with this sequence given the index
                updatedThreadContent : ThreadContent
                updatedThreadContent =
                    case model.currentThread of
                        Just currentThread ->
                            List.indexedMap
                                (\i item ->
                                    if i == index then
                                        Just newSequence

                                    else
                                        item
                                )
                                currentThread.content

                        Nothing ->
                            []

                newCurrentThread : Maybe Thread
                newCurrentThread =
                    Maybe.map (\thread -> { thread | content = updatedThreadContent }) model.currentThread

                updateThread : Thread -> Thread
                updateThread thread =
                    if index == thread.id then
                        case newCurrentThread of
                            Just x ->
                                x

                            Nothing ->
                                thread

                    else
                        thread

                newThreads : List Thread
                newThreads =
                    List.map updateThread model.threads

                -- update thread with new thread content
                newPrinciples : List Ponder
                newPrinciples =
                    List.append [ Tuple.first newSequence ] model.principles
            in
            ( { model | currentThread = newCurrentThread, threads = newThreads, principles = newPrinciples }, Cmd.none )

        RevokePrinciple index ponder ->
            let
                newSequence : Sequence
                newSequence =
                    case ponder of
                        Reason text ->
                            ( ponder, Just text )

                        Principle text ->
                            ( Reason text, Just text )

                        Empty ->
                            ( ponder, Just "" )

                -- update threadcontent with this sequence given the index
                updatedThreadContent : ThreadContent
                updatedThreadContent =
                    case model.currentThread of
                        Just currentThread ->
                            List.indexedMap
                                (\i item ->
                                    if i == index then
                                        Just newSequence

                                    else
                                        item
                                )
                                currentThread.content

                        Nothing ->
                            []

                newCurrentThread : Maybe Thread
                newCurrentThread =
                    Maybe.map (\thread -> { thread | content = updatedThreadContent }) model.currentThread

                updateThread : Thread -> Thread
                updateThread thread =
                    if index == thread.id then
                        case newCurrentThread of
                            Just x ->
                                x

                            Nothing ->
                                thread

                    else
                        thread

                newThreads : List Thread
                newThreads =
                    List.map updateThread model.threads

                -- update thread with new thread content
                newPrinciples : List Ponder
                newPrinciples =
                    List.filter (\x -> x /= ponder) model.principles
            in
            ( { model | currentThread = newCurrentThread, threads = newThreads, principles = newPrinciples }, Cmd.none )

        MakeSelectedNode index ->
            ( { model | currentNodeIdx = index }, Cmd.none )

        MakeSelectedThread thread ->
            ( { model | currentThread = Just thread }, Cmd.none )

        CreateNewSequence currentThread ->
            let
                newList : ThreadContent
                newList =
                    List.append currentThread.content [ Just ( Reason "", Just "" ) ]

                newCurrentThread : Maybe Thread
                newCurrentThread =
                    Maybe.map (\thread -> { thread | content = newList }) model.currentThread

                updateThread : Thread -> Thread
                updateThread thread =
                    if currentThread.id == thread.id then
                        case newCurrentThread of
                            Just x ->
                                x

                            Nothing ->
                                thread

                    else
                        thread

                newThreads : List Thread
                newThreads =
                    List.map updateThread model.threads
            in
            ( { model | currentThread = newCurrentThread, threads = newThreads }, Cmd.none )

        GiveReason index reasonText ->
            let
                updatedTuple : Sequence
                updatedTuple =
                    ( Reason reasonText, Just reasonText )

                updatedThreadContent : ThreadContent
                updatedThreadContent =
                    case model.currentThread of
                        Just currentThread ->
                            if index /= 0 && reasonText == "" then
                                removeAt index currentThread.content

                            else
                                List.indexedMap
                                    (\i item ->
                                        if i == index then
                                            Just updatedTuple

                                        else
                                            item
                                    )
                                    currentThread.content

                        Nothing ->
                            []

                updatedName : String
                updatedName =
                    if index == 0 then
                        reasonText

                    else
                        case model.currentThread of
                            Just x ->
                                x.name

                            Nothing ->
                                ""

                newCurrentThread : Maybe Thread
                newCurrentThread =
                    Maybe.map (\thread -> { thread | content = updatedThreadContent, name = updatedName }) model.currentThread

                updateThread : Thread -> Thread
                updateThread thread =
                    case model.currentThread of
                        Just currentThread ->
                            if currentThread.id == thread.id then
                                case newCurrentThread of
                                    Just x ->
                                        x

                                    Nothing ->
                                        thread

                            else
                                thread

                        Nothing ->
                            thread

                newThreads : List Thread
                newThreads =
                    List.map updateThread model.threads
            in
            ( { model | currentThread = newCurrentThread, threads = newThreads }, Cmd.none )

        DeletePrinciple _ ->
            Debug.todo "branch 'DeletePrinciple _' not implemented"



-- COMMANDS


doNothingCmd : Cmd Msg
doNothingCmd =
    Cmd.none



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- PORTS


port changeTheme : String -> Cmd msg



-- UTILITIES
-- Define the `last` function


last : List a -> Maybe a
last list =
    list
        |> List.head


threadSpacingStyle : String
threadSpacingStyle =
    "grid gap-4"


flattenList : List (List a) -> List a
flattenList nested =
    List.concat nested
