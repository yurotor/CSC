
open System
open CSC.IO
open Crypto
open CSC
open Wallet
open Blockchain
open CSC.Model.Miner
open Suave
open Suave.Filters
open Suave.WebSocket
open Suave.Operators
open Suave.Sockets
open Suave.Sockets.Control
open System.Configuration

type Command =
    | GetBalance of Key
    | Pay of Key * Key * uint64
    | GetTransactions of Key
    | Pubkey of Key
    | NewWallet of string
    | Undefined

let parseCommand (cmd: string)  =
    let parts = cmd.Split(' ')
    match parts |> List.ofArray with
    | tp :: key :: _ when tp = "balance" -> GetBalance (Convert.FromBase64String(key))
    | tp :: from :: to_ :: am :: _ when tp = "tx" -> 
        let mutable amount: uint64 = 0UL
        if UInt64.TryParse(am, &amount) then Pay (Convert.FromBase64String(from), Convert.FromBase64String(to_), amount)
        else Undefined
    | tp :: key :: _ when tp = "transactions" -> GetTransactions (Convert.FromBase64String(key))
    | tp :: key :: _ when tp = "pubkey" -> Pubkey (Convert.FromBase64String(key))
    | tp :: name :: _ when tp = "newwallet" -> NewWallet name
    | _ -> Undefined

//ukeselman
//09V9BX8ikB7P/CDFXWghDWKD/uyrmOLsPdt2bWiS+bU=
//A1QOL+SG0FJcxqABrE58PO2as1CeO6eAsv9yCJclNE0s

//ukeselman2
//yeVS/rBAIVETw/KiLhi3QTZoBp7QlSs9Q3Mp/W8Qm8c= 
//AtvsszMjn1tMD5Xb+cpKglgw3hBg1tVoWFrdomW6/Mbq

//18446744073709551615UL
//4073709551615UL

let mutable server: Server = 
    Server(
        saveBlock ConfigurationManager.AppSettings.["Storage"], 
        Miner(), 
        Convert.ToUInt64(ConfigurationManager.AppSettings.["Threshold"]))//744073709551615UL

let ws (webSocket : WebSocket) _ =
    socket {
        let mutable loop = true
        while loop do
            try 
                let! msg = webSocket.read()
                match msg with
                | (Text, data, true) ->
                    match data |> UTF8.toString |> parseCommand with
                    | GetBalance key -> 
                        let response =
                            key 
                            |> createPubKeyBytes
                            |> server.GetBalance
                            |> (sprintf "balance %i")
                            |> System.Text.Encoding.UTF8.GetBytes
                            |> ByteSegment
                        do! webSocket.send Text response true
            
                    | Pay (from, to_, amount) ->
                        let response = 
                            match server.Pay from to_ amount with
                            | Ok _ -> sprintf "payok"
                            | Error e -> sprintf "payerr %s" e
                            |> System.Text.Encoding.UTF8.GetBytes
                            |> ByteSegment
                        do! webSocket.send Text response true
                
                    | GetTransactions key ->
                        let response = 
                            key
                            |> createPubKeyBytes
                            |> server.GetTransactions
                            |> List.sortByDescending (fun t -> t.time)
                            |> List.map Blockchain.describeUserTransaction
                            |> (fun list -> String.Join(" ", list |> List.toArray))
                            |> sprintf "transactions %s" 
                            |> System.Text.Encoding.UTF8.GetBytes
                            |> ByteSegment
                        do! webSocket.send Text response true

                    | Pubkey key ->
                        let response = 
                            key
                            |> createPubKeyBytes
                            |> (fun k -> Convert.ToBase64String(k))
                            |> sprintf "pubkey %s" 
                            |> System.Text.Encoding.UTF8.GetBytes
                            |> ByteSegment
                        do! webSocket.send Text response true

                    | NewWallet name ->
                        let wallet = { Wallet.name = name; Wallet.key = createPrivateKeyBytes }
                        Async.RunSynchronously <| saveWallet ConfigurationManager.AppSettings.["Storage"] wallet
                        let response = 
                            sprintf "wallet %s %s" wallet.name (Convert.ToBase64String(wallet.key))
                            |> System.Text.Encoding.UTF8.GetBytes
                            |> ByteSegment
                        do! webSocket.send Text response true

                    | _ -> ()

                | (Close, _, _) ->
                    let emptyResponse = [||] |> ByteSegment
                    do! webSocket.send Close emptyResponse true
                    loop <- false
                | _ -> ()
            with
            | (exc: Exception) ->
                printfn "%s" (exc.Message)
                let response = exc.Message |> sprintf "error %s" |> System.Text.Encoding.UTF8.GetBytes |> ByteSegment
                do! webSocket.send Close response true
    }

let app: WebPart =
    choose [
        path "/websocket" >=> handShake ws
    ]

let mutable continueLooping = true

[<EntryPoint>]
let main _ =
    try
        let minerWallet = loadWallet ConfigurationManager.AppSettings.["MinerWallet"]
        let blockchain = loadBlockchain ConfigurationManager.AppSettings.["Storage"]

        server.InitBlocks blockchain
        Async.Start <| server.Start minerWallet.key
        Async.Start <| async { startWebServer defaultConfig app }
    
        while continueLooping do
            match Console.ReadLine() with
            | "q" -> 
                continueLooping <- false
                server.Stop
            | _ -> 
                ()
            
    with 
    | (exc: Exception) -> 
        printfn "%s" (exc.Message)
        printfn "Press any key to exit."
        Console.ReadLine() |> ignore
    0