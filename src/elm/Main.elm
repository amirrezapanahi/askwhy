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
import Html.Events exposing (onClick)



-- CONSTANTS


ask_why_description : String
ask_why_description =
    "by asking a series of questions and reaching primitive principles, you can determine if whatever you are putting forward time and energy towards is a worthwhile investment."



-- MESSAGES


type Msg
    = CreateThread
    | DeleteThread String



-- MODEL


type alias Model =
    { count : Int
    , threads : List Thread
    , current_thread : Maybe Thread
    , principles : List Principle
    }


type alias Reason =
    String


type alias Why =
    String


type alias Principle =
    String


type alias Name =
    String


type alias Thread =
    { name : String
    , content : Node
    }


type Ponder
    = Why
    | Reason
    | Principle
    | Empty


type Node
    = Node
        { value : Ponder
        , next : Maybe Node
        }



-- INIT


initialModel : ( Model, Cmd Msg )
initialModel =
    ( { count = 0
      , threads = []
      , current_thread = Nothing
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
            [ Attr.class "font-semibold" ]
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
            , div [ Attr.class "h-full w-3/5" ]
                [ span [ Attr.class ("bg-green-200 " ++ titleStyles) ] [ text "Current Thread" ]
                , currentThreadView model.current_thread
                ]
            , div [ Attr.class "h-full bg-blue-200 w-1/5" ]
                [ text "Principles" ]
            ]
        ]


currentThreadView : Maybe Thread -> Html Msg
currentThreadView maybeThread =
    case maybeThread of
        Just thread ->
            div [ Attr.class "grid grid-cols-[1fr_auto_1fr]" ]
                [ span [ Attr.class "text-right italic" ] [ text "Start here:" ], span [] [ text "" ], input [ Attr.value thread.name ] [] ]

        Nothing ->
            text ""


threadListView : Model -> Html Msg
threadListView model =
    div [] (List.map threadView model.threads)


threadView : Thread -> Html Msg
threadView thread =
    div [ Attr.class "flex flex-row justify-around" ]
        [ span
            []
            [ text thread.name ]
        , button [ onClick (DeleteThread thread.name) ] [ text "×" ]
        ]


principleListView : Model -> Html Msg
principleListView model =
    div [] (List.map principleView model.principles)


principleView : Principle -> Html Msg
principleView principle =
    div [] [ text principle ]


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
                    Thread "Untitled" (Node { value = Empty, next = Nothing })
            in
            ( { model | current_thread = Just newThread, threads = newThread :: model.threads }, Cmd.none )

        DeleteThread threadToDeleteName ->
            let
                currentThreadBeingDeleted : String -> Bool
                currentThreadBeingDeleted name =
                    case model.current_thread of
                        Just thread ->
                            if name == thread.name then
                                True

                            else
                                False

                        Nothing ->
                            False

                removeThreadFromList : List Thread
                removeThreadFromList =
                    List.filter
                        (\thread ->
                            thread.name /= threadToDeleteName
                        )
                        model.threads
            in
            if currentThreadBeingDeleted threadToDeleteName then
                ( { model
                    | current_thread = Nothing
                    , threads = removeThreadFromList
                  }
                , Cmd.none
                )

            else
                ( { model | threads = removeThreadFromList }, Cmd.none )



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
