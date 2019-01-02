module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { todos =
            [ Todo 0 "牛乳を買う" False High
            ]
      , filter = All
      , draftTitle = ""
      , draftImportance = Mid
      }
    , Cmd.none
    )


type alias Model =
    { todos : List Todo
    , filter : Filter
    , draftTitle : String
    , draftImportance : Importance
    }


type alias Todo =
    { id : Int
    , title : String
    , isCompleted : Bool
    , importance : Importance
    }


type Importance
    = High
    | Mid
    | Low


type Filter
    = All
    | NotCompleted
    | Completed


type Msg
    = NoOp
    | UpdateTitle String
    | UpdateImportance String
    | AddTodo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        UpdateTitle title ->
            ( { model | draftTitle = title }, Cmd.none )

        UpdateImportance value ->
            let
                newImportance : Importance
                newImportance =
                    case value of
                        "High" ->
                            High

                        "Mid" ->
                            Mid

                        "Low" ->
                            Low

                        _ ->
                            Mid
            in
            ( { model | draftImportance = newImportance }, Cmd.none )

        AddTodo ->
            let
                newId : Int
                newId =
                    case List.reverse model.todos |> List.head of
                        Just lastTodo ->
                            lastTodo.id + 1

                        Nothing ->
                            1

                newTodo : Todo
                newTodo =
                    { id = newId
                    , title = model.draftTitle
                    , isCompleted = False
                    , importance = model.draftImportance
                    }
            in
            ( { model
                | todos = newTodo :: List.reverse model.todos |> List.reverse
                , draftTitle = ""
                , draftImportance = Mid
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    let
        form : Html Msg
        form =
            div []
                [ div []
                    [ input
                        [ type_ "text"
                        , onInput UpdateTitle
                        , value model.draftTitle
                        ]
                        []
                    , select [ onInput UpdateImportance ]
                        [ option [ value "High", selected (model.draftImportance == High) ] [ text "High" ]
                        , option [ value "Mid", selected (model.draftImportance == Mid) ] [ text "Mid" ]
                        , option [ value "Low", selected (model.draftImportance == Low) ] [ text "Low" ]
                        ]
                    ]
                , div []
                    [ button [ onClick AddTodo ] [ text "ADD" ] ]
                ]

        todoItem : Todo -> Html Msg
        todoItem todo =
            li []
                [ input [ type_ "checkbox", checked todo.isCompleted ] []
                , p [] [ text todo.title ]
                ]
    in
    { title = "Elm Todo"
    , body =
        [ form
        , ul [] (List.map todoItem model.todos)
        ]
    }
