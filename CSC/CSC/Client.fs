﻿namespace CSC

open Blockchain
open System
open CSC.IO

module Client =
    let mutable continueLooping = true
    let mutable blocks = []
    let mutable monitor = new System.Object()
    let mutable mempool: Transaction list = []

    let start key =
        continueLooping <- true
        async {
            while continueLooping do
                lock monitor (fun () ->
                    let getMempoolTransactions max = 
                        let touse, toremain =
                            mempool
                            |> List.indexed
                            |> List.partition (fst >> (>) max)
                        mempool <- toremain |> List.map snd
                        touse |> List.map snd

                    let time = DateTime.Now
                    let newBlock =
                        Miner.mine 
                            key
                            blocks
                            (getMempoolTransactions 1000)
                            time
                            4073709551615UL//18446744073709551615UL
                            1UL
                    match newBlock with
                    | Some block -> 
                        let count = ((blocks |> List.length) + 1)
                        printfn "Mined block #%i with nonce=%A after %A" count block.nonce (DateTime.Now.Subtract(time))
                        match block.transactions with
                        | _ :: rest -> rest |> List.iter (fun tx -> printfn "Transaction %s" (describe tx))
                        | _ -> ()
                        
                        blocks <- block :: blocks
                        saveBlock block count
                    | _ -> ()
                )
        }

    let stop =
        lock monitor (fun () -> continueLooping <- true)

    let tryPay pubkey amount =
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

    let getBalance pubkey =
        blocks
        |> getUTXOSet 
        |> List.filter (fun utxo -> utxo.output.pubKeyHash = hash pubkey)
        |> List.sumBy (fun u -> u.output.value) 

    let pay transaction =
        lock monitor (fun () -> mempool <- transaction :: mempool)




                    

