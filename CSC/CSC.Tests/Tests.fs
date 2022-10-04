module Tests

open System
open System.Collections.Generic
open Xunit
open FsUnit
open NBitcoin.Secp256k1
open System.Text
open Crypto
open Utils
open Blockchain
open CSC
open CSC.Model.Miner


    [<Fact>]
    let ``Create private key returns a 32 byte key`` () =
        let pubkey = createPrivateKeyBytes
        pubkey |> Array.length |> should equal 32

    [<Fact>]
     let ``Create public key returns a 33 byte key`` () =
        let prkey = createPrivateKeyBytes
        let pubkey = createPubKeyBytes prkey
        pubkey |> Array.length |> should equal 33

    [<Fact>]
    let ``Public key can be recreated from bytes`` () =
        let prkey = createPrivateKeyBytes
        let pubkey = createPubKey prkey
        let pubkeyBytes = createPubKeyBytes prkey
        let mutable pubkey2: ECPubKey = null 
        let mutable compressed = true
        if ECPubKey.TryCreate(System.Span<byte> pubkeyBytes, null, &compressed, &pubkey2) then
            pubkey = pubkey2 |> should equal true
        else
            failwith "ECPubKey.TryCreate failed"

    [<Fact>]
    let ``Wallet save and load keeps all keys`` () =
        let mutable storage = ""
        let wallet = { Wallet.name="ukeselman";Wallet.key=createPrivateKeyBytes }
        Wallet.save (fun w -> storage <- w) Serializer.serialize wallet
        let wallet2 = Wallet.load (fun _ -> storage) Serializer.deserialize<Wallet.Wallet> wallet.name
        wallet.key |> should equal wallet2.key

    [<Fact>]
    let ``Create blockchain with 10 blocks`` () =
        let key = createPrivateKeyBytes         
        let blocks = createBlockchain 10 key
        blocks |> List.length |> should equal 10

    [<Fact>]
    let ``Verify signature`` () =
        let randomStr = 
            let chars = "ABCDEFGHIJKLMNOPQRSTUVWUXYZ0123456789"
            let charsLen = chars.Length
            let random = System.Random()
        
            fun len -> 
                let randomChars = [|for i in 0..len-1 -> chars.[random.Next(charsLen)]|]
                new System.String(randomChars)

        let msg = bytesOf <| randomStr 32
        let prkey = createPrivateKeyBytes 
        let pubkeyBytes = createPubKeyBytes prkey

        match sign prkey msg with
        | Some s -> verifySig s pubkeyBytes msg |> should equal true
        | _ -> failwith "Failed to sign message"

    [<Fact>]
    let ``Validate blockchain`` () =
        let key = createPrivateKeyBytes   
        let blocks = createBlockchainWithThreshold 10 key defaultThreshold
        validateBlockchain defaultThreshold blocks |> should equal Valid

    [<Fact>]
    let ``Check balance after payment`` () =
        let minerKey = Convert.FromBase64String("Zpv9z9EWnUIXbejcDbpjW8Ed/fDr80ltN38M/Hpa1+8=")
        let receiverKey = Convert.FromBase64String("yeVS/rBAIVETw/KiLhi3QTZoBp7QlSs9Q3Mp/W8Qm8c=")  
        let receiverPubkey = createPubKeyBytes receiverKey
        let payerKey = createPrivateKeyBytes
        let payerPubKey = createPubKeyBytes payerKey
        payerKey |> should not' (equal receiverKey)
        let miner = defaultMiner ()
        let amount = 10UL
        let blocks = createBlockchainWithThreshold 1 payerKey defaultThreshold
        let balanceBefore = Wallet.getBalance payerPubKey blocks
        match Wallet.tryPay blocks payerPubKey amount with
        | Ok (utxos, total) ->
            let tx = Wallet.buildTransaction (unixTime DateTime.Now) utxos total payerKey receiverPubkey amount
            match miner.Mine minerKey blocks [tx] DateTime.Now defaultThreshold with
            | Some block ->
                let balanceAfter = Wallet.getBalance payerPubKey (List.rev (block :: blocks))
                balanceAfter |> should equal (balanceBefore - amount)
            | _ -> failwith "Failed to mine new block"
        | Error e -> failwith e

    [<Fact>]
    let ``Verify miner transactions`` () =
        let payerKey = createPrivateKeyBytes
        let payerPubKey = createPubKeyBytes payerKey
        let blocks = createBlockchainWithThreshold 2 payerKey defaultThreshold
        let server = defaultServer ()
        server.InitBlocks blocks
        let txcount = server.GetTransactions payerPubKey |> List.filter (fun t -> t.type_ = Mined) |> List.length
        txcount |> should equal 2
                
    [<Fact>]
    let ``Verify paid transaction is in mempool`` () =
        let receiverKey = Convert.FromBase64String("yeVS/rBAIVETw/KiLhi3QTZoBp7QlSs9Q3Mp/W8Qm8c=")  
        let receiverPubkey = createPubKeyBytes receiverKey
        let payerKey = createPrivateKeyBytes
        let amount = 10UL
        let blocks = createBlockchainWithThreshold 1 payerKey defaultThreshold
        let server = defaultServer ()
        let countBefore = server.GetTransactions receiverPubkey |> List.filter (fun t -> t.type_ = Incoming) |> List.length
        server.InitBlocks blocks
        match server.Pay payerKey receiverPubkey amount with
        | Ok _ ->
            let transactions = server.GetTransactions receiverPubkey |> List.filter (fun t -> t.type_ = Incoming)
            transactions |> List.length |> should equal (countBefore + 1)
            match transactions with
            | t :: _ -> t.confirmed |> should equal false
            | _ -> failwith "Transaction not found"
        | Error e -> failwith e


    [<Fact>]
    let ``Verify paid transaction is in blockchain`` () =
        let receiverKey = Convert.FromBase64String("yeVS/rBAIVETw/KiLhi3QTZoBp7QlSs9Q3Mp/W8Qm8c=")  
        let receiverPubkey = createPubKeyBytes receiverKey
        let payerKey = createPrivateKeyBytes
        let payerPubKey = createPubKeyBytes payerKey
        let amount = 10UL
        let miner = defaultMiner ()
        let blocks = createBlockchainWithThreshold 1 payerKey defaultThreshold
        let server = defaultServer ()
        let countBefore = server.GetTransactions receiverPubkey |> List.filter (fun t -> t.type_ = Incoming) |> List.length
        match Wallet.tryPay blocks payerPubKey amount with
        | Ok (utxos, total) ->
            let tx = Wallet.buildTransaction (unixTime DateTime.Now) utxos total payerKey receiverPubkey amount
            match miner.Mine payerKey blocks [tx] DateTime.Now defaultThreshold with
            | Some block ->
                server.InitBlocks (block :: blocks)
                let transactions = server.GetTransactions receiverPubkey |> List.filter (fun t -> t.type_ = Incoming)
                transactions |> List.length |> should equal (countBefore + 1)
                match transactions with
                | t :: _ -> t.confirmed |> should equal true
                | _ -> failwith "Block mining failed"
            | _ -> failwith "Transaction not found"
        | Error e -> failwith e

    [<Fact>]
    let ``Verify outgoing transactions in mempool`` () =
        let receiverKey = Convert.FromBase64String("yeVS/rBAIVETw/KiLhi3QTZoBp7QlSs9Q3Mp/W8Qm8c=")  
        let receiverPubkey = createPubKeyBytes receiverKey
        let payerKey = createPrivateKeyBytes
        let payerPubKey = createPubKeyBytes payerKey
        let amount = 210UL
        let blocks = createBlockchainWithThreshold 3 payerKey defaultThreshold
        let server = defaultServer ()
        server.InitBlocks blocks
        Async.Start <| server.Start payerKey
        match server.Pay payerKey receiverPubkey amount with
        | Ok tx ->             
            let tx = server.GetTransactions payerPubKey |> List.filter (fun t -> t.type_ = Outgoing)
            tx |> List.length |> should equal 1
            match tx with
            | t :: _ -> t.amount |> should equal amount
            | _ -> failwith "Transaction not found"
        | Error e -> failwith e

    [<Fact>]
    let ``Verify outgoing transactions in blockchain`` () =
        let receiverKey = Convert.FromBase64String("yeVS/rBAIVETw/KiLhi3QTZoBp7QlSs9Q3Mp/W8Qm8c=")  
        let receiverPubkey = createPubKeyBytes receiverKey
        let payerKey = createPrivateKeyBytes
        let payerPubKey = createPubKeyBytes payerKey
        let amount = 210UL
        let miner = defaultMiner ()
        let blocks = createBlockchainWithThreshold 3 payerKey defaultThreshold
        let server = defaultServer ()
        match Wallet.tryPay blocks payerPubKey amount with
        | Ok (utxos, total) ->
            let tx = Wallet.buildTransaction (unixTime DateTime.Now) utxos total payerKey receiverPubkey amount
            match miner.Mine payerKey blocks [tx] DateTime.Now defaultThreshold with
            | Some block ->
                server.InitBlocks (block :: blocks)
                payerPubKey
                |> server.GetTransactions  
                |> List.filter (fun t -> t.type_ = Outgoing)
                |> List.length 
                |> should equal 1
            | _ -> failwith "Transaction not found"
        | Error e -> failwith e

    
    [<Fact>]
    let ``Verify incoming transactions without change in mempool`` () =
        let receiverKey = Convert.FromBase64String("yeVS/rBAIVETw/KiLhi3QTZoBp7QlSs9Q3Mp/W8Qm8c=")  
        let receiverPubkey = createPubKeyBytes receiverKey
        let payerKey = createPrivateKeyBytes
        let payerPubKey = createPubKeyBytes payerKey
        let amount = 10UL
        let blocks = createBlockchainWithThreshold 1 payerKey defaultThreshold
        let server = defaultServer ()
        server.InitBlocks blocks
        match server.Pay payerKey receiverPubkey amount with
        | Ok _ ->             
            payerPubKey
            |> server.GetTransactions  
            |> List.filter (fun t -> t.type_ = Incoming && not t.confirmed)
            |> List.length 
            |> should equal 0
        | Error e -> failwith e

    [<Fact>]
    let ``Verify incoming transactions without change in blockchain`` () =
        let receiverKey = Convert.FromBase64String("yeVS/rBAIVETw/KiLhi3QTZoBp7QlSs9Q3Mp/W8Qm8c=")  
        let receiverPubkey = createPubKeyBytes receiverKey
        let payerKey = createPrivateKeyBytes
        let amount = 10UL
        let blocks = createBlockchainWithThreshold 1 payerKey defaultThreshold
        let server = defaultServer ()
        server.InitBlocks blocks
        Async.Start <| server.Start payerKey
        match server.Pay payerKey receiverPubkey amount with
        | Ok _ ->   
            Notifications.waitFor 10
            let tx = server.GetTransactions receiverPubkey |> List.filter (fun t -> t.type_ = Incoming && t.confirmed)
            tx |> List.length |> should equal 1
            match tx with
            | t :: _ -> t.amount |> should equal amount
            | _ -> failwith "Transaction not found"     
        | Error e -> failwith e

    [<Fact>]
    let ``Verify transactions of both sides`` () =
        let receiverKey = Convert.FromBase64String("yeVS/rBAIVETw/KiLhi3QTZoBp7QlSs9Q3Mp/W8Qm8c=")  
        let receiverPubkey = createPubKeyBytes receiverKey
        let payerKey = createPrivateKeyBytes
        let payerPubKey = createPubKeyBytes payerKey
        let amount = 10UL
        let blocks = createBlockchainWithThreshold 1 payerKey defaultThreshold
        let server = defaultServer ()
        server.InitBlocks blocks
        Async.Start <| server.Start payerKey
        match server.Pay payerKey receiverPubkey amount with
        | Ok _ ->   
            Notifications.waitFor 15
            
            let rectx = server.GetTransactions receiverPubkey |> List.filter (fun t -> t.type_ = Incoming && t.confirmed)
            rectx |> List.length |> should equal 1

            let sentx = server.GetTransactions payerPubKey |> List.filter (fun t -> t.type_ = Outgoing && t.confirmed)
            sentx |> List.length |> should equal 1

            match rectx, sentx with
            | t1 :: _, t2 :: _ -> t1.amount |> should equal t2.amount
            | _ -> failwith "Transaction not found"      
            
        | Error e -> failwith e

    [<Fact>]
    let ``Compressed sensing result is reproducable`` () =
        let s = "data"
        let bytes = hash (System.Text.Encoding.ASCII.GetBytes(s))
        let cs1 = CompressedSensing.calculate 4 bytes
        let cs2 = CompressedSensing.calculate 4 bytes
        FSharp.Stats.Matrix.Generic.compare cs1 cs2 |> should equal 0

    [<Fact>]
    let ``Compressed sensing result varies for small input changes`` () =
        let s = "data"
        let bytes = hash (System.Text.Encoding.ASCII.GetBytes(s))
        let s2 = "datb"
        let bytes2 = hash (System.Text.Encoding.ASCII.GetBytes(s2))
        let cs1 = CompressedSensing.calculate 4 bytes
        let cs2 = CompressedSensing.calculate 4 bytes2
        FSharp.Stats.Matrix.Generic.compare cs1 cs2 |> should not' (equal 0)

    [<Fact>]
    let ``Verify block content`` () =
        let blocks = createBlockchainWithThreshold 1 createPrivateKeyBytes defaultThreshold
        match blocks with
        | block :: _ -> 
            let bytes = getBlockContent block.transactions
            bytes |> should equal block.content
        | _ -> failwith "Block not found"

    [<Fact>]
    let ``Verify no change output when change is 0`` () =
        let receiverKey = Convert.FromBase64String("yeVS/rBAIVETw/KiLhi3QTZoBp7QlSs9Q3Mp/W8Qm8c=")  
        let receiverPubkey = createPubKeyBytes receiverKey
        let payerKey = createPrivateKeyBytes
        let amount = 100UL
        let blocks = createBlockchainWithThreshold 1 payerKey defaultThreshold
        let server = defaultServer ()
        server.InitBlocks blocks
        match server.Pay payerKey receiverPubkey amount with
        | Ok tx ->             
            tx.outputs |> List.length |> should equal 1
        | Error e -> failwith e

    [<Fact>]
    let ``Find incorrect values in blockchain`` () =
        let payerKey = createPrivateKeyBytes
        let receiverKey = Convert.FromBase64String("yeVS/rBAIVETw/KiLhi3QTZoBp7QlSs9Q3Mp/W8Qm8c=")  
        let txbuilder = (fun bs ->
            if bs |> List.length > 0 then 
                buildTransactions bs payerKey receiverKey 10UL
            else [])
        let blocks = buildBlockchain txbuilder 2 payerKey defaultThreshold

        let hackValues =
            List.map 
                (fun (b: Block) -> 
                    { b with 
                        transactions = 
                            b.transactions 
                            |> List.map 
                                (fun t -> 
                                    if t.inputs |> List.length = 0 then t 
                                    else 
                                        { t with 
                                            outputs = 
                                                t.outputs 
                                                |> List.map (fun o -> { o with value = o.value * 10UL })}
                                )
                    }
                )

        blocks
        |> hackValues
        |> validateBlockchain defaultThreshold 
        |> ValidationResult.compare 
            (Invalid 
                [ {error = ""; type_ = TransactionAmountInvalid };
                  {error = ""; type_ = BlockContentMismatch } ]
            )
        |> should equal true

    [<Fact>]
    let ``Find incorrect signatures in blockchain`` () =
        let payerKey = createPrivateKeyBytes
        let receiverKey = Convert.FromBase64String("yeVS/rBAIVETw/KiLhi3QTZoBp7QlSs9Q3Mp/W8Qm8c=")  
        let txbuilder = (fun bs ->
            if bs |> List.length > 0 then 
                buildTransactions bs payerKey receiverKey 10UL
            else [])
        let blocks = buildBlockchain txbuilder 2 payerKey defaultThreshold

        let replaceByte bytes =
            match bytes |> List.ofArray with
            | head :: tail -> (head + 1uy) :: tail
            | _ -> []
            |> Array.ofList

        let hackSigs =
            List.map 
                (fun (b: Block) -> 
                    { b with 
                        transactions = 
                            b.transactions 
                            |> List.map 
                                (fun t -> 
                                    { t with 
                                        inputs = 
                                            t.inputs 
                                            |> List.map 
                                                (fun i ->                                                    
                                                    { i with signature = replaceByte i.signature }
                                                )
                                    }
                                )
                    }
                )

        blocks
        |> hackSigs
        |> validateBlockchain defaultThreshold 
        |> ValidationResult.compare 
            (Invalid 
                [ {error = ""; type_ = SignatureMismatch };
                  {error = ""; type_ = BlockContentMismatch } ]
            )
        |> should equal true

    [<Fact>]
    let ``Verify previous block header hash`` () =
        let payerKey = createPrivateKeyBytes
        let receiverKey = Convert.FromBase64String("yeVS/rBAIVETw/KiLhi3QTZoBp7QlSs9Q3Mp/W8Qm8c=")  
        let txbuilder = (fun bs ->
            if bs |> List.length > 0 then 
                buildTransactions bs payerKey receiverKey 10UL
            else [])
        let blocks = buildBlockchain txbuilder 2 payerKey defaultThreshold

        let replaceByte bytes =
            match bytes |> List.ofArray with
            | head :: tail -> (head + 1uy) :: tail
            | _ -> []
            |> Array.ofList

        let hackBlockHeaders =
            List.map 
                (fun (b: Block) -> 
                    { b with prevBlockHeaderHash = replaceByte b.prevBlockHeaderHash }
                )

        blocks
        |> hackBlockHeaders
        |> validateBlockchain defaultThreshold 
        |> ValidationResult.compare (Invalid  [ {error = ""; type_ = BlockHeaderHashMismatch } ])
        |> should equal true
    
    [<Fact>]
    let ``Test payment to third party`` () =
        let receiverKey = Convert.FromBase64String("yeVS/rBAIVETw/KiLhi3QTZoBp7QlSs9Q3Mp/W8Qm8c=")  
        let receiverPubkey = createPubKeyBytes receiverKey
        let receiverKey2 = Convert.FromBase64String("Js0/WDqKp9QAV7Txe/l11UGfrjE92S3SvrQ5khBKGHU=")  
        let receiverPubkey2 = createPubKeyBytes receiverKey2
        let payerKey = createPrivateKeyBytes
        let amount = 50UL
        let blocks = createBlockchainWithThreshold 1 payerKey defaultThreshold
        let server = defaultServer ()
        server.InitBlocks blocks
        Async.Start <| server.Start payerKey
        match server.Pay payerKey receiverPubkey amount with
        | Ok _ ->   
            Notifications.waitFor 10

            let amount2 = amount / 2UL
            match server.Pay receiverKey receiverPubkey2 amount2 with
            | Ok _ ->
                Notifications.waitFor 50

                let rectx = server.GetTransactions receiverPubkey2 |> List.filter (fun t -> t.type_ = Incoming && t.confirmed)
                let sentx = server.GetTransactions receiverPubkey |> List.filter (fun t -> t.type_ = Outgoing && t.confirmed)
                match sentx, rectx with
                | [ t1 ], [ t2 ] -> t1.amount |> should equal t2.amount
                | _ -> failwith "Transaction not found"
            | Error e -> failwith e
          
        | Error e -> failwith e

    [<Fact>]
    let ``Verify blockchain validity`` () =
        let payerKey = createPrivateKeyBytes
        let mutable blocks: Block list = []
        let server = Server((fun b _ -> blocks <- b :: blocks), defaultMiner (), defaultThreshold)
        Async.Start <| server.Start payerKey
        Notifications.waitFor 20
        validateBlockchain defaultThreshold (blocks |> List.rev) 
        |> ValidationResult.compare Valid        
        |> should equal true