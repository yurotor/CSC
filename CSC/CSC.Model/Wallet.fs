module Wallet

open NBitcoin.Secp256k1
open System
open Blockchain
open Crypto
    
    type Wallet = {
        name: string
        key: Key
    }

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

    let tryPay blocks pubkey amount =
        result {
            let utxos = getUTXOSet blocks
            let myutxos = 
                utxos
                |> List.filter (fun utxo -> utxo.output.pubKeyHash = hash pubkey)
                |> List.sortByDescending (fun utxo -> utxo.output.value)
            let useutxos =
                myutxos
                |> List.fold 
                    (fun lst utxo ->
                        if lst |> List.sumBy (fun u -> u.output.value) >= amount then lst
                        else utxo :: lst
                    )
                    []
            let total = useutxos |> List.sumBy (fun u -> u.output.value) 
            if total < amount then return! Error "Not enough funds"
            else return! Ok (useutxos, total)
        }

    let buildTransaction pubkey time utxos total from to_ amount =
        let inputs =
            utxos
            |> List.choose (createTransactionInput from)
        let output =
            createTransactionOutput to_ amount
        let change =
            createTransactionOutput pubkey (total - amount)
        createTransaction inputs [output; change] time

    let getBalance pubkey blocks =
        let utxos = getUTXOSet blocks
        let myutxos = 
            utxos
            |> List.filter (fun utxo -> utxo.output.pubKeyHash = hash pubkey)
        myutxos |> List.sumBy (fun u -> u.output.value)
        //blocks
        //|> getUTXOSet 
        //|> List.filter (fun utxo -> utxo.output.pubKeyHash = hash pubkey)
        //|> List.sumBy (fun u -> u.output.value) 