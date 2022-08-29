module Wallet

open NBitcoin.Secp256k1
open System
open Blockchain
open Crypto
    
    type Wallet = {
        name: string
        keys: Key list
    }

    let addKeyToWallet key wallet =
        { wallet with keys = key :: wallet.keys }

    let save saver serializer wallet =
        wallet |> serializer |> saver

    let load loader deserializer walletName = 
        walletName |> loader |> deserializer

    let outputHash output =
        Array.concat [bytesOf (output.value.ToString()); output.pubKeyHash] |> hash

    let createCoinbaseTransaction value time key =
        let pubKeyHash = key |> createPubKeyBytes |> hash
        { inputs = []; outputs = [{ value = value; pubKeyHash = pubKeyHash }]; time = time }

    let createTransaction inputs outputs time =
        { inputs = inputs; outputs = outputs; time = time }

    let createTransactionInput key (utxo: UTXO) =
        let prevTxId = outputHash utxo.output
        sign key (toSign prevTxId utxo.index)
        |> Option.map 
            (fun signature -> 
                { prevTxId = prevTxId; 
                  prevTxIndex = utxo.index; 
                  pubKey = createPubKeyBytes key; 
                  signature = signature })

    let createTransactionOutput key value =
        { value = value; pubKeyHash = key |> createPubKeyBytes |> hash }

