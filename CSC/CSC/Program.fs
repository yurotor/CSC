
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

let run txcount =
    //generate n transactions
    let time = unixTime DateTime.Now
    let key = createPrivateKeyBytes
    let txs = [1..txcount] |> List.map (fun i -> Wallet.createCoinbaseTransaction (uint64(i)) time key)
    //hash them using CS and compute avg time
    let avg =
        [1..100]
        |> List.map
            (fun _ ->
                let start = DateTime.Now
                txs |> getBlockContent |> ignore
                let span = DateTime.Now.Subtract(start)
                span.TotalMilliseconds
            )
        |> List.average
    
    printfn "Compressed sensing: Average time is %A for %i transactions" avg txcount

    //hash them using Merkle tree and compute avg time
    let input = txs |> List.map (toBytes >> hash)
    let rec compute (list: byte array list) =
        let res =
            list
            |> List.chunkBySize 2
            |> List.map (Array.concat >> hash)
            |> function
                | [x] -> x
                | (_ :: _ :: _) as list -> compute list
                | _ -> [||]
        
        res

    
    let avg =
        [1..100]
        |> List.map
            (fun _ ->
                let start = DateTime.Now
                let _ = compute input
                let span = DateTime.Now.Subtract(start)
                span.TotalMilliseconds
            )
        |> List.average

    printfn "Merkle tree: Average time is %A for %i transactions" avg txcount

    ()

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

let ws  (server: Server) (webSocket : WebSocket) (context: HttpContext) =
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

let app server: WebPart =
    choose [
        path "/websocket" >=> handShake (ws server)
    ]

let mutable continueLooping = true

[<EntryPoint>]
let main _ =
    //run 10
    //run 100
    //run 1000
    //Console.ReadLine() |> ignore
    //0
    try
        let minerWallet = loadWallet ConfigurationManager.AppSettings.["MinerWallet"]
        let storage = ConfigurationManager.AppSettings.["Storage"]
        let threshold = Convert.ToUInt64(ConfigurationManager.AppSettings.["Threshold"])
        let blockchain = loadBlockchain storage

        let server = Server(saveBlock storage, Miner(), threshold)//744073709551615UL

        do
            match blockchain |> validateBlockchain threshold with
            | Valid -> 
                match blockchain |> List.length with
                | 0 -> ()
                | x -> printfn "Valid blockchain of length %i" x
                server.InitBlocks (blockchain |> List.rev)
                Async.Start <| server.Start minerWallet.key
                Async.Start <| async { startWebServer defaultConfig (app server) }
    
                while continueLooping do
                    match Console.ReadLine() with
                    | "q" -> 
                        continueLooping <- false
                        server.Stop
                    | _ -> 
                        ()
            | Invalid e ->
                printfn "Invalid blockchain. Reasons - %s" (e.ToString())
                printfn "Press any key to exit."
                ()
            
    with 
    | (exc: Exception) -> 
        printfn "%s" (exc.Message)
        printfn "Press any key to exit."
        Console.ReadLine() |> ignore
    0