module Bingo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Random
import Http
import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Decode.Pipeline exposing (decode, required, hardcoded, optional)
import Json.Encode as Encode


-- Model


type GameState
    = EnteringName
    | Playing


type alias Model =
    { name : String
    , gameNumber : Int
    , entries : List Entry
    , alertMessage : Maybe String
    , nameInput : String
    , gameState : GameState
    }


type alias Entry =
    { id : Int
    , phrase : String
    , points : Int
    , marked : Bool
    }


type alias Score =
    { id : Int
    , name : String
    , score : Int
    }


initialModel : Model
initialModel =
    { name = "Anonymous"
    , gameNumber = 1
    , entries = []
    , alertMessage = Nothing
    , nameInput = ""
    , gameState = EnteringName
    }


apiUrlPrefix : String
apiUrlPrefix =
    "http://localhost:3000"



-- Update


type Msg
    = NewGame
    | Mark Int
    | Sort
    | NewRandom Int
    | NewEntries (Result Http.Error (List Entry))
    | CloseAlert
    | ShareScore
    | NewScore (Result Http.Error Score)
    | SetNameInput String
    | SaveName
    | CancelName
    | ChangeGameState GameState


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeGameState state ->
            ( { model | gameState = state }, Cmd.none )

        SaveName ->
            if String.isEmpty model.nameInput then
                ( model, Cmd.none )
            else
                ( { model
                    | name = model.nameInput
                    , nameInput = ""
                    , gameState = Playing
                  }
                , Cmd.none
                )

        CancelName ->
            ( { model
                | nameInput = ""
                , gameState = Playing
              }
            , Cmd.none
            )

        SetNameInput value ->
            ( { model | nameInput = value }, Cmd.none )

        NewRandom randomNumber ->
            ( { model | gameNumber = randomNumber }, Cmd.none )

        NewGame ->
            { model | gameNumber = model.gameNumber + 1 } ! [ getEntries ]

        ShareScore ->
            ( model, postScore model )

        NewScore (Ok score) ->
            let
                message =
                    "Your score of "
                        ++ (toString score.score)
                        ++ " was successfully shared!"
            in
                ( { model | alertMessage = Just message }, Cmd.none )

        NewScore (Err error) ->
            let
                message =
                    "Error posting your score:"
                        ++ (toString error)
            in
                ( { model | alertMessage = Just message }, Cmd.none )

        NewEntries result ->
            case result of
                Ok randomEntries ->
                    ( { model | entries = List.sortBy .points randomEntries }, Cmd.none )

                Err error ->
                    let
                        errorMessage =
                            case error of
                                Http.NetworkError ->
                                    "Is the server running?"

                                Http.BadStatus response ->
                                    (toString response.status)

                                Http.BadPayload message _ ->
                                    "Decoding Failed: " ++ message

                                _ ->
                                    (toString error)
                    in
                        ( { model | alertMessage = Just (toString errorMessage) }, Cmd.none )

        CloseAlert ->
            ( { model | alertMessage = Nothing }, Cmd.none )

        Mark id ->
            let
                markEntry e =
                    if e.id == id then
                        { e | marked = (not e.marked) }
                    else
                        e
            in
                ( { model | entries = List.map markEntry model.entries }, Cmd.none )

        Sort ->
            ( { model | entries = List.sortBy .points model.entries }, Cmd.none )



-- Decoders/Encoders


entryDecoder : Decoder Entry
entryDecoder =
    decode Entry
        |> Json.Decode.Pipeline.required "id" Decode.int
        |> Json.Decode.Pipeline.required "phrase" Decode.string
        |> Json.Decode.Pipeline.required "points" Decode.int
        |> hardcoded False


entryListDecoder : Decoder (List Entry)
entryListDecoder =
    Decode.list entryDecoder


encodeScore : Model -> Encode.Value
encodeScore model =
    Encode.object
        [ ( "name", Encode.string model.name )
        , ( "score", Encode.int (sumMarkedPoints model.entries) )
        ]


scoreDecoder : Decoder Score
scoreDecoder =
    Decode.map3 Score
        (field "id" Decode.int)
        (field "name" Decode.string)
        (field "score" Decode.int)



-- Commands


postScore : Model -> Cmd Msg
postScore model =
    let
        url =
            apiUrlPrefix ++ "/scores"

        body =
            encodeScore model
                |> Http.jsonBody

        request =
            Http.post url body scoreDecoder
    in
        Http.send NewScore request


generateRandomNumber : Cmd Msg
generateRandomNumber =
    Random.generate NewRandom (Random.int 1 100)


getEntries : Cmd Msg
getEntries =
    entryListDecoder
        |> Http.get (apiUrlPrefix ++ "/random-entries")
        |> Http.send NewEntries



-- View


sumMarkedPoints : List Entry -> Int
sumMarkedPoints entries =
    entries
        |> List.filter .marked
        |> List.foldl (\entry sum -> sum + entry.points) 0


viewScore : Int -> Html Msg
viewScore sum =
    div
        [ class "score" ]
        [ span [ class "label" ] [ text "Score" ]
        , span [ class "value" ] [ text (toString sum) ]
        ]


allEntriesMarked : List Entry -> Bool
allEntriesMarked entries =
    List.all .marked entries


viewPlayer : String -> Int -> Html Msg
viewPlayer name gameNumber =
    h2 [ id "info", class "classy" ]
        [ a [ href "#", onClick (ChangeGameState EnteringName) ]
            [ text name ]
        , text (" - Game #" ++ (toString gameNumber))
        ]


viewHeader : String -> Html Msg
viewHeader title =
    header []
        [ h1 [] [ text title ] ]


viewFooter : Html Msg
viewFooter =
    footer []
        [ a [ href "http://elm-lang.org" ]
            [ text "Powered By Elm" ]
        ]


viewEntryItem : Entry -> Html Msg
viewEntryItem entry =
    li [ classList [ ( "marked", entry.marked ) ], onClick (Mark entry.id) ]
        [ span [ class "phrase" ] [ text entry.phrase ]
        , span [ class "points" ] [ text (toString entry.points) ]
        ]


viewEntryList : List Entry -> Html Msg
viewEntryList entries =
    entries
        |> List.map viewEntryItem
        |> ul []


hasZeroScore : Model -> Bool
hasZeroScore model =
    (sumMarkedPoints model.entries) == 0


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewHeader "BUZZWORD BINGO"
        , viewPlayer model.name model.gameNumber
        , viewAlertMessage model.alertMessage
        , viewNameInput model
        , viewEntryList model.entries
        , viewScore (sumMarkedPoints model.entries)
        , div [ class "button-group" ]
            [ button [ onClick NewGame ] [ text "New Game" ]
            , button [ onClick ShareScore, disabled (hasZeroScore model) ] [ text "Share Score" ]
            ]
        , viewFooter
        ]


viewNameInput : Model -> Html Msg
viewNameInput model =
    case model.gameState of
        EnteringName ->
            div [ class "name-input" ]
                [ input
                    [ type_ "text"
                    , placeholder "Who's playing?"
                    , autofocus True
                    , onInput SetNameInput
                    , value model.nameInput
                    ]
                    []
                , button [ onClick SaveName ] [ text "Save" ]
                , button [ onClick CancelName ] [ text "Cancel" ]
                ]

        Playing ->
            text ""


viewAlertMessage : Maybe String -> Html Msg
viewAlertMessage alertMessage =
    case alertMessage of
        Just message ->
            div [ class "alert" ]
                [ span [ class "close", onClick CloseAlert ] [ text "X" ]
                , text message
                ]

        Nothing ->
            text ""


init : ( Model, Cmd Msg )
init =
    initialModel ! [ getEntries ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (always Sub.none)
        }
