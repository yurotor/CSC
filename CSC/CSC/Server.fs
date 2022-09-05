namespace CSC

open Blockchain
open System
open Common.Crypto
open Wallet

[<AutoOpen>]
module Server =
    
    type Server(persistance) =
        let mutable blockPersistance: (Block -> int -> unit) = persistance
        let mutable monitor = new System.Object()
        let mutable continueLooping = true
        let mutable blocks: Block list = []
        let mutable mempool: Transaction list = []
        
        member _.Start key =
            continueLooping <- true
            async {
                while continueLooping do
                    lock monitor (fun () ->
                        let getMempoolTransactions max = 
                            let touse, toremain =
                                mempool
                                |> List.indexed
                                |> List.partition (fst >> (>) max)
                            touse |> List.map snd, toremain |> List.map snd

                        let time = DateTime.Now
                        let blockTransactions, newMempool = getMempoolTransactions 1000                        
                        let newBlock =
                            Miner.mine 
                                key
                                blocks
                                blockTransactions
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
                            
                            mempool <- newMempool
                            blocks <- block :: blocks
                            blockPersistance block count
                        | _ -> ()
                    )
            }

        member _.Stop =
            lock monitor (fun () -> continueLooping <- true)

        member _.InitBlocks initialBlocks =
            lock monitor (fun () -> blocks <- initialBlocks)

        member _.TryPay pubkey amount =
            Wallet.tryPay blocks pubkey amount

        member private _.PayInner transaction =
            lock monitor 
                (fun () -> 
                    mempool <- transaction :: mempool
                    let sum = transaction.outputs |> List.sumBy (fun o -> o.value) 
                    printfn "New transaction entered the mempool - %i $" sum
                    transaction
                )

        member this.Pay key topubkey amount =
            let pubkey = createPubKeyBytes key
            amount
            |> this.TryPay pubkey
            |> Result.map 
                (fun (utxos, total) ->                            
                    amount
                    |> buildTransaction (unixTime DateTime.Now) utxos total key topubkey  
                    |> this.PayInner
                )

        member _.GetBalance pubkey =
            let utxos = getUTXOSet blocks
            let myutxos = 
                utxos
                |> List.filter (fun utxo -> utxo.output.pubKeyHash = hash pubkey)
            myutxos |> List.sumBy (fun u -> u.output.value)

        member _.GetIncomingTransactions pubkey =
            let confirmed =
                blocks
                |> List.map (fun block -> block.transactions |> List.map (fun t -> t.outputs |> List.map (fun o -> o, t)) |> List.concat)
                |> List.concat
                |> List.filter (fun (output, _) -> output.pubKeyHash = hash pubkey)
                |> List.map 
                    (fun (output, tx) -> 
                        let tp = if tx.inputs |> List.isEmpty then Mined else Incoming
                        { confirmed = true; amount = output.value; address = Convert.ToBase64String(output.pubKeyHash); type_ = tp }
                    )
                    
            let unconfirmed = 
                mempool
                |> List.map (fun t -> t.outputs)
                |> List.concat
                |> List.filter (fun output -> output.pubKeyHash = hash pubkey)
                |> List.map (fun output -> { confirmed = false; amount = output.value; address = Convert.ToBase64String(output.pubKeyHash); type_ = Incoming })

            confirmed @ unconfirmed

        member _.GetOutgoingTransactions pubkey =
            let confirmed =
                blocks
                |> List.mapi 
                    (fun blockIndex block -> 
                        block.transactions 
                        |> List.map (fun t -> t.inputs) 
                        |> List.concat 
                        |> List.map (fun input -> input, blockIndex))
                |> List.concat
                |> List.filter (fun (input, _) -> input.pubKey = pubkey)
                |> List.choose 
                    (fun (input, blockIndex) -> 
                        input 
                        |> findOutputByInput (blocks |> List.take blockIndex)
                        |> Option.map (fun output -> { confirmed = true; amount = output.value; address = Convert.ToBase64String(output.pubKeyHash); type_ = Outgoing }))
                    
            let unconfirmed = 
                mempool
                |> List.map (fun t -> t.outputs)
                |> List.concat
                |> List.filter (fun output -> output.pubKeyHash = hash pubkey)
                |> List.map (fun output -> { confirmed = false; amount = output.value; address = Convert.ToBase64String(output.pubKeyHash); type_ = Outgoing })

            confirmed @ unconfirmed

        member this.GetTransactions pubkey =
            this.GetIncomingTransactions pubkey @ this.GetOutgoingTransactions pubkey