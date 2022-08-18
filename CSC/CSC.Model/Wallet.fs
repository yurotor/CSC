module Wallet

open NBitcoin.Secp256k1
open System
open Blockchain
    
    type Wallet = {
        name: string
        keys: Key list
    }

    let addKeyToWallet key wallet =
        { wallet with keys = key :: wallet.keys }

    let createPrivateKeyBytes =        
        Key.generate

    let createPrivateKey privKey =
        ECPrivKey.Create (ReadOnlySpan<byte> privKey)

    let createPubKey privKey =
        let pr = createPrivateKey privKey
        pr.CreatePubKey ()     

    let createPubKeyBytes privKey =
        let pubkey = createPubKey privKey
        let mutable buffer: Span<byte> = Span<byte> (Array.zeroCreate 33)
        let mutable len = 0
        pubkey.WriteToSpan (true, buffer, &len)
        buffer.ToArray ()

    let save saver serializer wallet =
        wallet |> serializer |> saver

    let load loader deserializer walletName = 
        walletName |> loader |> deserializer

    let createCoinbaseTransaction value time key =
        let pubKeyHash = key |> createPubKeyBytes |> hash
        { inputs = []; outputs = [{ value = value; pubKeyHash = pubKeyHash }]; time = time }

    let createTransaction inputs outputs time =
        { inputs = inputs; outputs = outputs; time = time }

    let createTransactionInput (spend: Transaction) index wallet =
        match spend.outputs |> List.tryItem index, wallet.keys with
        | Some utxo, key :: _ ->
            let prevTxId = Array.concat [bytesOf (utxo.value.ToString()); utxo.pubKeyHash] |> hash
            let prKey = createPrivateKey key
            let toSign = Array.concat [prevTxId; bytesOf (index.ToString())]
            let mutable signature: SecpECDSASignature = null
            if prKey.TrySignECDSA(ReadOnlySpan<byte> toSign, &signature) then
                let mutable buffer: Span<byte> = Span<byte> (Array.zeroCreate 32)
                signature.WriteCompactToSpan(buffer)
                Some { prevTxId = prevTxId; prevTxIndex = index; pubKey = createPubKeyBytes key; signature = buffer.ToArray () }
            else
                None
        | _ -> None

    let createTransactionOutput value wallet =
        match wallet.keys with
        | key :: _ -> Some { value = value; pubKeyHash = key |> createPubKeyBytes |> hash }
        | _ -> None

