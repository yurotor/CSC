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
        //match wallet.key, wallet2.key with
        //| k1::k12::_, k21::k22::_ when k11=k21 && k12=k22 -> true
        //| _ -> failwith "Wallets don't contain keys"

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
        let threshold = 18446744073709551615UL
        let blocks = createBlockchainWithThreshold 10 key threshold
        validateBlockchain threshold blocks |> should equal Valid

    [<Fact>]
    let ``Check balance after payment`` () =
        let key = createPrivateKeyBytes 
        let pubkey = createPubKeyBytes key
        let receiverKeys = ("yeVS/rBAIVETw/KiLhi3QTZoBp7QlSs9Q3Mp/W8Qm8c=", "AtvsszMjn1tMD5Xb+cpKglgw3hBg1tVoWFrdomW6/Mbq")
        let payto = receiverKeys |> snd |> (fun x -> Convert.FromBase64String(x))
        let amount = 10UL
        let threshold = 18446744073709551615UL
        let blocks = createBlockchainWithThreshold 1 key threshold
        let balanceBefore = Wallet.getBalance pubkey blocks
        match Wallet.tryPay blocks pubkey amount with
        | Ok (utxos, total) ->
            let tx = Wallet.buildTransaction pubkey (unixTime DateTime.Now) utxos total key payto amount
            match Miner.mine key blocks [tx] DateTime.Now threshold 1UL with
            | Some block ->
                let balanceAfter = Wallet.getBalance pubkey (block :: blocks)
                balanceAfter |> should equal (balanceBefore - amount)
            | _ -> failwith "Failed to mine new block"
        | Error e -> failwith e
                
