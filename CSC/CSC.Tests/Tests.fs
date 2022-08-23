module Tests

open System
open System.Collections.Generic
open Xunit
open FsUnit
open NBitcoin.Secp256k1
open System.Text
open Crypto

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
        let wallet = { Wallet.name="ukeselman";Wallet.keys=[createPrivateKeyBytes;createPrivateKeyBytes] }
        Wallet.save (fun w -> storage <- w) Serializer.serialize wallet
        let wallet2 = Wallet.load (fun _ -> storage) Serializer.deserialize<Wallet.Wallet> wallet.name
        match wallet.keys, wallet2.keys with
        | k11::k12::_, k21::k22::_ when k11=k21 && k12=k22 -> true
        | _ -> failwith "Wallets don't contain keys"

    [<Fact>]
    let ``Create blockchain with 10 blocks`` () =
        let key = createPrivateKeyBytes 
        let rec mine blocks threshold nonce lim =
            let time = unixTime DateTime.Now
            let tx = Wallet.createCoinbaseTransaction 100UL time key
            let prevBlock = blocks |> List.tryHead
            match Blockchain.tryCreateBlock Serializer.txid prevBlock [ tx ] time threshold nonce with
            | Some block -> 
                if lim > (blocks |> List.length) then mine (block :: blocks) threshold nonce lim
                else (block :: blocks)
            | _ -> mine blocks threshold (Blockchain.nextNonce nonce) lim
        
        let blocks = mine [] 1 1UL 9
        blocks |> List.length |> should equal 10