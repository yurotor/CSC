
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
open System.IO


//Wallet.save (fun w -> File.WriteAllText(wallet.name, w)) Serializer.serialize wallet

//let wallet2 = 
//    Wallet.load (fun name -> File.ReadAllText(name)) Serializer.deserialize<Wallet.Wallet> wallet.name



//let block = Blockchain.tryCreateBlock None [ tx ] time 1 1UL

//let rec mine blocks threshold nonce lim =
//    let time = unixTime DateTime.Now
//    let tx = Wallet.createCoinbaseTransaction 100UL time key
//    let prevBlock = blocks |> List.tryHead
//    match Blockchain.tryCreateBlock prevBlock [ tx ] time threshold nonce with
//    | Some block -> 
//        if lim > (blocks |> List.length) then mine (block :: blocks) threshold nonce lim
//        else (block :: blocks)
//    | _ -> mine blocks threshold (Blockchain.nextNonce nonce) lim

//let blocks = mine [] 1 1UL 10

//blocks
//|> List.iteri
//    (fun i b ->
//        Blockchain.save 
//            (fun s -> 
//                let path = Path.Combine(Directory.GetCurrentDirectory(), "Blockchain", (sprintf "%i.dat" i))
//                File.WriteAllText(path, s)
//            ) 
//            b
//    )

//let blocks = loadBlockchain
//let newBlock =
//    Miner.mine 
//        key
//        blocks
//        DateTime.Now
//        1
//        1UL

//match newBlock with
//| Some block -> saveBlock block ((blocks |> List.length) + 1)
//| _ -> ()
    //Blockchain.load 
    //    (fun i ->
    //        let path = Path.Combine(Directory.GetCurrentDirectory(), "Blockchain", (sprintf "%i.dat" i))
    //        if File.Exists(path) then
    //            File.ReadAllText(path) |> Some
    //        else
    //            None
    //    )

let mutable continueLooping = true

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

let txcmd (cmd:string) =
    //tx from to amount
    let parts = cmd.Split(' ')
    let mutable amount: uint64 = 0UL
    if parts |> Array.length = 4 && parts.[0] = "tx" && UInt64.TryParse(parts.[3], &amount) then
        Some (Convert.FromBase64String(parts.[1]), Convert.FromBase64String(parts.[2]), amount)
    else
        None

//ukeselman
//09V9BX8ikB7P/CDFXWghDWKD/uyrmOLsPdt2bWiS+bU=
//A1QOL+SG0FJcxqABrE58PO2as1CeO6eAsv9yCJclNE0s

//ukeselman2
//yeVS/rBAIVETw/KiLhi3QTZoBp7QlSs9Q3Mp/W8Qm8c= 
//AtvsszMjn1tMD5Xb+cpKglgw3hBg1tVoWFrdomW6/Mbq

//18446744073709551615UL
//4073709551615UL

let mutable server: Server = Server(saveBlock, Miner(), 44073709551615UL)

let ws (webSocket : WebSocket) (context: HttpContext) =
    socket {
        let mutable loop = true
        while loop do
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
                    Async.Start <| 
                        Wallet.save 
                            (fun w -> async { File.WriteAllText(wallet.name, w) }) 
                            Serializer.serialize 
                            wallet
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
    }

let app: WebPart =
    choose [
        path "/websocket" >=> handShake ws
    ]

[<EntryPoint>]
let main _ =
    let wallet = load (fun name -> System.IO.File.ReadAllText(name)) Serializer.deserialize<Wallet.Wallet> "ukeselman"
    let minerKey = wallet.key

    Async.Start <| server.Start minerKey
    Async.Start <| async { startWebServer defaultConfig app }
    

    //let k = createPrivateKeyBytes
    //let pk = createPubKeyBytes k
    //let wallet = { Wallet.name="ukeselman2";Wallet.keys=[k;pk] }
    //Wallet.save (fun w -> System.IO.File.WriteAllText(wallet.name, w)) Serializer.serialize wallet

    while continueLooping do
        match Console.ReadLine() with
        | "q" -> 
            continueLooping <- false
            server.Stop
        | _ -> 
            let t = "balance A1QOL+SG0FJcxqABrE58PO2as1CeO6eAsv9yCJclNE0s"
            let cmd = parseCommand t
            match cmd with
            | GetBalance k -> 
                printfn "%i" (server.GetBalance k)
            | _ -> ()
            //"tx 09V9BX8ikB7P/CDFXWghDWKD/uyrmOLsPdt2bWiS+bU= AtvsszMjn1tMD5Xb+cpKglgw3hBg1tVoWFrdomW6/Mbq 10" 
            //match txcmd t with
            //| Some (from, to_, amount) ->
            //    let pubkey = createPubKeyBytes from
            //    match Client.tryPay pubkey amount with
            //    | Ok (utxos, total) -> 
            //        let inputs =
            //            utxos
            //            |> List.choose (createTransactionInput from)
            //        let output =
            //            createTransactionOutput to_ amount
            //        let change =
            //            createTransactionOutput pubkey (total - amount)
            //        let tx = createTransaction inputs [output; change] (unixTime DateTime.Now)

            //        Client.pay tx

            //    | Error e -> printfn "%s" e
            //| _ -> ()

    

    0