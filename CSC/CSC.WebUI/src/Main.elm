port module Main exposing (..)

import Bool.Extra exposing (fromString)
import Browser
import Codec exposing (Codec)
import DateTime exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Select as SelectFile
import Html exposing (Html)
import Html.Attributes as Html
import Html.Events as Html
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
    { wallet : Maybe Wallet
    , balance : Int
    , to : String
    , amount : String
    , paymentStatus : PaymentStatus
    , transactions : List Transaction
    , pubkey : Maybe String
    , newWalletName : Maybe String
    , walletSelector : WalletSelector
    , changeWallet : Bool
    }


type WalletSelector
    = New
    | Existing


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
                                    ((*) 1000
                                        >> Time.millisToPosix
                                        >> DateTime.fromPosix
                                        >> Transaction c t a add
                                    )

                        _ ->
                            Nothing

                _ ->
                    Nothing
        )


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model
        Nothing
        0
        ""
        ""
        None
        []
        Nothing
        Nothing
        New
        False
    , Cmd.none
    )



-- UPDATE


type Msg
    = Recv String
    | Tick Time.Posix
    | ToChanged String
    | AmountChanged String
    | PayClicked
    | WalletSelectorChanged WalletSelector
    | NewWalletName String
    | CreateNewWallet
    | ChangeWallet
    | SelectWalletFile
    | GotWalletFile File
    | MarkdownLoaded String



-- Use the `sendMessage` port when someone presses ENTER or clicks
-- the "Send" button. Check out index.html to see the corresponding
-- JS where this is piped into a WebSocket.
--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Recv message ->
            handleResponse model message

        Tick _ ->
            let
                cmd =
                    model.wallet
                        |> Maybe.map
                            (.key
                                >> (\key ->
                                        Cmd.batch
                                            [ sendMessage ("balance " ++ key)
                                            , sendMessage ("transactions " ++ key)
                                            ]
                                   )
                            )
                        |> Maybe.withDefault Cmd.none
            in
            ( model, cmd )

        ToChanged to ->
            ( { model | to = to }, Cmd.none )

        AmountChanged amount ->
            ( { model | amount = amount }, Cmd.none )

        PayClicked ->
            let
                ( m, cmd ) =
                    case ( model.wallet, model.amount |> String.toInt ) of
                        ( Just wallet, Just _ ) ->
                            ( { model | paymentStatus = Waiting }
                            , sendMessage ("tx " ++ wallet.key ++ " " ++ model.to ++ " " ++ model.amount)
                            )

                        _ ->
                            ( model, Cmd.none )
            in
            ( m, cmd )

        NewWalletName name ->
            ( { model | newWalletName = Just name, changeWallet = False }, Cmd.none )

        CreateNewWallet ->
            let
                cmd =
                    case model.newWalletName of
                        Just name ->
                            if name |> String.isEmpty then
                                Cmd.none

                            else
                                sendMessage ("newwallet " ++ name)

                        _ ->
                            Cmd.none
            in
            ( { model | changeWallet = False }, cmd )

        WalletSelectorChanged ws ->
            ( { model | walletSelector = ws }, Cmd.none )

        ChangeWallet ->
            ( { model | changeWallet = True }, Cmd.none )

        SelectWalletFile ->
            ( model, SelectFile.file [] GotWalletFile )

        GotWalletFile file ->
            ( model, Task.perform MarkdownLoaded (File.toString file) )

        MarkdownLoaded data ->
            let
                ( wallet, cmd ) =
                    case data |> Codec.decodeString walletDecoder of
                        Ok w ->
                            ( w, Cmd.batch [ sendMessage ("balance " ++ w.key), sendMessage ("pubkey " ++ w.key) ] )

                        Err _ ->
                            ( Wallet "invalid" "", Cmd.none )
            in
            ( { model | wallet = Just wallet, changeWallet = False }
            , cmd
            )


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

                ( "pubkey", p :: _ ) ->
                    ( { model | pubkey = Just p }, Cmd.none )

                ( "wallet", name :: key :: _ ) ->
                    ( { model | wallet = Just <| Wallet name key }, sendMessage ("pubkey " ++ key) )

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
        , Time.every 1000 Tick
        ]



-- VIEW


view : Model -> Html Msg
view model =
    layout [ width fill, height fill ] <|
        column [ padding 20, spacing 20 ]
            (viewWalletSelector model
                :: (case model.wallet of
                        Just wallet ->
                            [ text ("Wallet: " ++ wallet.name)
                            , text ("Balance: " ++ String.fromInt model.balance ++ " $")
                            , row [ spacing 10 ]
                                [ text "Get Paid: "
                                , model.pubkey |> Maybe.map text |> Maybe.withDefault Element.none
                                ]
                            , row [ spacing 10 ]
                                [ Input.text []
                                    { onChange = ToChanged
                                    , text = model.to
                                    , placeholder = Just <| Input.placeholder [] <| text <| "To address"
                                    , label = Input.labelHidden ""
                                    }
                                , Input.text []
                                    { onChange = AmountChanged
                                    , text = model.amount
                                    , placeholder = Just <| Input.placeholder [] <| text <| "Amount"
                                    , label = Input.labelHidden ""
                                    }
                                , viewButton PayClicked "Pay"
                                ]
                            , viewTransactions model
                            ]

                        _ ->
                            []
                   )
            )


blue =
    Element.rgb 0 0 0.8


darkBlue =
    Element.rgb 0 0 0.9


white =
    Element.rgb 1 1 1


viewButton : Msg -> String -> Element Msg
viewButton msg txt =
    Input.button
        [ Background.color blue
        , Font.color white
        , Border.color darkBlue
        , paddingXY 32 16
        , Border.rounded 3
        ]
        { onPress = Just msg
        , label = text txt
        }


viewWalletSelector : Model -> Element Msg
viewWalletSelector model =
    let
        viewSelector =
            column [ spacing 20 ]
                (Input.radio
                    []
                    { onChange = WalletSelectorChanged
                    , options = [ Input.option New (text "New wallet"), Input.option Existing (text "Wallet from file") ]
                    , selected = Just model.walletSelector
                    , label = Input.labelLeft [ paddingEach { left = 0, right = 20, top = 0, bottom = 0 } ] (text "Select wallet")
                    }
                    :: (case model.walletSelector of
                            Existing ->
                                [ viewButton SelectWalletFile "Select wallet file"
                                ]

                            New ->
                                [ row [ spacing 10 ]
                                    [ Input.text []
                                        { onChange = NewWalletName
                                        , text = model.newWalletName |> Maybe.withDefault ""
                                        , placeholder = Just <| Input.placeholder [] <| text <| "Wallet name"
                                        , label = Input.labelHidden ""
                                        }
                                    , viewButton CreateNewWallet "Create new wallet"
                                    ]
                                ]
                       )
                )
    in
    case ( model.wallet, model.changeWallet ) of
        ( Just _, False ) ->
            viewButton ChangeWallet "Change wallet"

        _ ->
            viewSelector


viewTransactions : Model -> Element Msg
viewTransactions model =
    column [ spacing 10 ] <|
        List.map
            (\t ->
                row [ spacing 10 ]
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

        pad i =
            if i < 10 then
                "0" ++ String.fromInt i

            else
                String.fromInt i
    in
    (String.fromInt <| getDay date)
        ++ " "
        ++ (toEnglishMonth <| getMonth date)
        ++ " "
        ++ (String.fromInt <| getYear date)
        ++ " "
        ++ (pad <| getHours date)
        ++ ":"
        ++ (pad <| getMinutes date)
        ++ ":"
        ++ (pad <| getSeconds date)



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
