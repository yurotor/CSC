port module Main exposing (..)

import Bool.Extra exposing (fromString)
import Browser
import Codec exposing (Codec)
import DateTime exposing (..)
import File exposing (File)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D exposing (decodeString)
import Task exposing (Task)
import Time exposing (Month(..))



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


type PaymentStatus
    = None
    | Waiting
    | Paid
    | Failed String


type alias Wallet =
    { name : String
    , key : String
    }


type alias Model =
    { wallet : Wallet
    , balance : Int
    , to : String
    , amount : String
    , paymentStatus : PaymentStatus
    , transactions : List Transaction
    }


type TransactionType
    = Mined
    | Incoming
    | Outgoing


parseTransactionType : String -> Maybe TransactionType
parseTransactionType s =
    case s of
        "Mined" ->
            Just Mined

        "Incoming" ->
            Just Incoming

        "Outgoing" ->
            Just Outgoing

        _ ->
            Nothing


type alias Transaction =
    { confirmed : Bool
    , type_ : TransactionType
    , amount : Int
    , address : String
    , time : DateTime
    }


parseTransactions : List String -> List Transaction
parseTransactions =
    List.filterMap
        (\s ->
            case s |> String.split ";" of
                conf :: tp :: am :: add :: time :: _ ->
                    case ( conf |> fromString, tp |> parseTransactionType, am |> String.toInt ) of
                        ( Just c, Just t, Just a ) ->
                            time
                                |> String.toInt
                                |> Maybe.map
                                    (Time.millisToPosix
                                        >> DateTime.fromPosix
                                        >> Transaction c t a add
                                    )

                        _ ->
                            Nothing

                _ ->
                    Nothing
        )


init : () -> ( Model, Cmd Msg )
init flags =
    ( { wallet = Wallet "" "", balance = 0, to = "AtvsszMjn1tMD5Xb+cpKglgw3hBg1tVoWFrdomW6/Mbq", amount = "10", paymentStatus = None, transactions = [] }
    , Cmd.none
    )



-- UPDATE


type Msg
    = GotFiles (List File)
    | MarkdownLoaded String
    | Recv String
    | Tick Time.Posix
    | ToChanged String
    | AmountChanged String
    | PayClicked
    | GetTransactions



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
            handleResponse model message

        Tick _ ->
            let
                cmd =
                    if model.wallet.key |> String.isEmpty then
                        Cmd.none

                    else
                        sendMessage ("balance " ++ model.wallet.key)
            in
            ( model, cmd )

        ToChanged to ->
            ( { model | to = to }, Cmd.none )

        AmountChanged amount ->
            ( { model | amount = amount }, Cmd.none )

        PayClicked ->
            let
                ( m, cmd ) =
                    case model.amount |> String.toInt of
                        Just am ->
                            ( { model | paymentStatus = Waiting }
                            , sendMessage ("tx " ++ model.wallet.key ++ " " ++ model.to ++ " " ++ model.amount)
                            )

                        _ ->
                            ( model, Cmd.none )
            in
            ( m, cmd )

        GetTransactions ->
            let
                cmd =
                    if model.wallet.key |> String.isEmpty then
                        Cmd.none

                    else
                        sendMessage ("transactions " ++ model.wallet.key)
            in
            ( model, cmd )


handleResponse : Model -> String -> ( Model, Cmd Msg )
handleResponse model message =
    case message |> String.split " " of
        cmd :: prms ->
            case ( cmd, prms ) of
                ( "balance", bal :: _ ) ->
                    ( { model | balance = String.toInt bal |> Maybe.withDefault 0 }, Cmd.none )

                ( "payok", _ ) ->
                    ( { model | paymentStatus = Paid }, Cmd.none )

                ( "payerr", err :: _ ) ->
                    ( { model | paymentStatus = Failed err }, Cmd.none )

                ( "transactions", p ) ->
                    ( { model | transactions = parseTransactions p }, Cmd.none )

                _ ->
                    let
                        _ =
                            Debug.log "cmd" (cmd ++ (prms |> String.concat))
                    in
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS
-- Subscribe to the `messageReceiver` port to hear about messages coming in
-- from JS. Check out the index.html file to see how this is hooked up to a
-- WebSocket.
--


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ messageReceiver Recv
        , Time.every 30000 Tick
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
        , div []
            [ input [ placeholder "To address", value model.to, onInput ToChanged ] []
            , input [ placeholder "Amount", value model.amount, onInput AmountChanged ] []
            , button [ onClick PayClicked ] [ text "Pay" ]
            ]
        , div [] [ button [ onClick GetTransactions ] [ text "Get transactions" ] ]
        , div [] <|
            List.map
                (\t ->
                    div []
                        [ text <|
                            if t.confirmed then
                                "Confirmed"

                            else
                                "Unconfirmed"
                        , text <| ((String.fromInt <| t.amount) ++ " $")
                        , text <| dateToString t.time
                        ]
                )
            <|
                model.transactions
        ]


dateToString : DateTime -> String
dateToString date =
    let
        toEnglishMonth month =
            case month of
                Jan ->
                    "january"

                Feb ->
                    "february"

                Mar ->
                    "march"

                Apr ->
                    "april"

                May ->
                    "may"

                Jun ->
                    "june"

                Jul ->
                    "july"

                Aug ->
                    "august"

                Sep ->
                    "september"

                Oct ->
                    "october"

                Nov ->
                    "november"

                Dec ->
                    "december"
    in
    (String.fromInt <| getDay date)
        ++ " "
        ++ (toEnglishMonth <| getMonth date)
        ++ " "
        ++ (String.fromInt <| getYear date)
        ++ " "
        ++ (String.fromInt <| getHours date)
        ++ ":"
        ++ (String.fromInt <| getMinutes date)
        ++ ":"
        ++ (String.fromInt <| getSeconds date)



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
