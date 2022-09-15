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

    let createTransactionOutput pubkey value =
        { value = value; pubKeyHash = pubkey |> hash }

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

    let buildTransaction time utxos total payerKey receiverPubkey amount =
        let inputs =
            utxos
            |> List.choose (createTransactionInput payerKey)
        let output =
            createTransactionOutput receiverPubkey amount
        let outputs =
            if total - amount > 0UL then
                let change =
                    createTransactionOutput (createPubKeyBytes payerKey) (total - amount)
                [output; change]
            else
                [output]
        createTransaction inputs outputs time

    let getBalance pubkey =
        getUTXOSet
        >> List.filter (fun utxo -> utxo.output.pubKeyHash = hash pubkey)
        >> List.sumBy (fun u -> u.output.value)
        