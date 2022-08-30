port module Main exposing (..)

import Browser
import Codec exposing (Codec)
import File exposing (File)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D exposing (decodeString)
import Task exposing (Task)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- PORTS


port sendMessage : String -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg



-- MODEL


type alias Wallet =
    { name : String
    , keys : List String
    }


type alias Model =
    { draft : String
    , messages : List String
    , wallet : Wallet
    }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { draft = "", messages = [], wallet = Wallet "" [] }
    , Cmd.none
    )



-- UPDATE


type Msg
    = DraftChanged String
    | Send
    | Recv String
    | GotFiles (List File)
    | MarkdownLoaded String



-- Use the `sendMessage` port when someone presses ENTER or clicks
-- the "Send" button. Check out index.html to see the corresponding
-- JS where this is piped into a WebSocket.
--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DraftChanged draft ->
            ( { model | draft = draft }
            , Cmd.none
            )

        Send ->
            ( { model | draft = "" }
            , sendMessage model.draft
            )

        Recv message ->
            ( { model | messages = model.messages ++ [ message ] }
            , Cmd.none
            )

        GotFiles files ->
            let
                cmd =
                    case files of
                        file :: _ ->
                            Task.perform MarkdownLoaded (File.toString file)

                        _ ->
                            Cmd.none
            in
            ( model, cmd )

        --Task.perform MarkdownLoaded (File.toString file) )
        MarkdownLoaded data ->
            let
                wallet =
                    case data |> Codec.decodeString walletDecoder of
                        Ok w ->
                            w

                        Err _ ->
                            Wallet "invalid" []
            in
            ( { model | wallet = wallet }
            , sendMessage model.draft
            )



-- SUBSCRIPTIONS
-- Subscribe to the `messageReceiver` port to hear about messages coming in
-- from JS. Check out the index.html file to see how this is hooked up to a
-- WebSocket.
--


subscriptions : Model -> Sub Msg
subscriptions _ =
    messageReceiver Recv



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Echo Chat" ]
        , ul []
            (List.map (\msg -> li [] [ text msg ]) model.messages)
        , input
            [ type_ "text"
            , placeholder "Draft"
            , onInput DraftChanged
            , on "keydown" (ifIsEnter Send)
            , value model.draft
            ]
            []
        , button [ onClick Send ] [ text "Send" ]
        , input
            [ type_ "file"
            , multiple False
            , on "change" (D.map GotFiles filesDecoder)
            ]
            []
        , h1 [] [ text ("-" ++ model.wallet.name ++ "-") ]
        ]



-- DETECT ENTER


ifIsEnter : msg -> D.Decoder msg
ifIsEnter msg =
    D.field "key" D.string
        |> D.andThen
            (\key ->
                if key == "Enter" then
                    D.succeed msg

                else
                    D.fail "some other key"
            )


filesDecoder : D.Decoder (List File)
filesDecoder =
    D.at [ "target", "files" ] (D.list File.decoder)


walletDecoder : Codec Wallet
walletDecoder =
    Codec.object Wallet
        |> Codec.field "name" .name Codec.string
        |> Codec.field "keys" .keys (Codec.list Codec.string)
        |> Codec.buildObject
