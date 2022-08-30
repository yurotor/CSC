port module Main exposing (..)

import Browser
import Codec exposing (Codec)
import File exposing (File)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D exposing (decodeString)
import Task exposing (Task)
import Time



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
    , key : String
    }


type alias Model =
    { wallet : Wallet
    , balance : Int
    }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { wallet = Wallet "" "", balance = 0 }
    , Cmd.none
    )



-- UPDATE


type Msg
    = GotFiles (List File)
    | MarkdownLoaded String
    | Recv String
    | Tick Time.Posix



-- Use the `sendMessage` port when someone presses ENTER or clicks
-- the "Send" button. Check out index.html to see the corresponding
-- JS where this is piped into a WebSocket.
--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        MarkdownLoaded data ->
            let
                ( wallet, cmd ) =
                    case data |> Codec.decodeString walletDecoder of
                        Ok w ->
                            ( w, sendMessage ("balance " ++ w.key) )

                        Err _ ->
                            ( Wallet "invalid" "", Cmd.none )
            in
            ( { model | wallet = wallet }
            , cmd
            )

        Recv message ->
            ( { model | balance = String.toInt message |> Maybe.withDefault 0 }
            , Cmd.none
            )

        Tick _ ->
            let
                cmd =
                    if model.wallet.key |> String.isEmpty then
                        Cmd.none

                    else
                        sendMessage ("balance " ++ model.wallet.key)
            in
            ( model, cmd )



-- SUBSCRIPTIONS
-- Subscribe to the `messageReceiver` port to hear about messages coming in
-- from JS. Check out the index.html file to see how this is hooked up to a
-- WebSocket.
--


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ messageReceiver Recv
        , Time.every 1000 Tick
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input
            [ type_ "file"
            , multiple False
            , on "change" (D.map GotFiles filesDecoder)
            ]
            []
        , h1 [] [ text ("Wallet: " ++ model.wallet.name) ]
        , h1 [] [ text ("Balance: " ++ String.fromInt model.balance ++ " $") ]
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
        |> Codec.field "key" .key Codec.string
        |> Codec.buildObject
