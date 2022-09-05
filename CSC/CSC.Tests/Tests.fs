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
        
        let amount = 10UL
        let blocks = createBlockchainWithThreshold 1 payerKey defaultThreshold
        let balanceBefore = Wallet.getBalance payerPubKey blocks
        match Wallet.tryPay blocks payerPubKey amount with
        | Ok (utxos, total) ->
            let tx = Wallet.buildTransaction (unixTime DateTime.Now) utxos total payerKey receiverPubkey amount
            match Miner.mine minerKey blocks [tx] DateTime.Now defaultThreshold 1UL with
            | Some block ->
                let balanceAfter = Wallet.getBalance payerPubKey (List.rev (block :: blocks))
                balanceAfter |> should equal (balanceBefore - amount)
            | _ -> failwith "Failed to mine new block"
        | Error e -> failwith e
                
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
        let blocks = createBlockchainWithThreshold 1 payerKey defaultThreshold
        let server = defaultServer ()
        let countBefore = server.GetTransactions receiverPubkey |> List.filter (fun t -> t.type_ = Incoming) |> List.length
        match Wallet.tryPay blocks payerPubKey amount with
        | Ok (utxos, total) ->
            let tx = Wallet.buildTransaction (unixTime DateTime.Now) utxos total payerKey receiverPubkey amount
            match Miner.mine payerKey blocks [tx] DateTime.Now defaultThreshold 1UL with
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
    let ``Verify outgoing transactions`` () =
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
        | Ok tx ->             
            let tx = server.GetTransactions payerPubKey |> List.filter (fun t -> t.type_ = Outgoing)
            tx |> List.length |> should equal 1
            match tx with
            | t :: _ -> t.amount |> should equal amount
            | _ -> failwith "Transaction not found"
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