namespace CSC

open Blockchain
open System
open Common.Crypto
open Wallet
open CSC.Model.Miner

[<AutoOpen>]
module Server =
    
    type Server(persistance, miner: Miner, threshold) =
        let mutable blockPersistance: (Block -> int -> unit) = persistance
        let mutable monitor = new System.Object()
        let mutable mempoolLocker = new System.Object()
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
                                |> List.partition (fun (i, _) -> i < max)
                            touse |> List.map snd, toremain |> List.map snd

                        let time = DateTime.Now
                        let blockTransactions = 
                            lock mempoolLocker (fun () ->
                                let blockTransactions, newMempool = getMempoolTransactions 1000  
                                mempool <- newMempool
                                blockTransactions
                            )
                        let newBlock =
                            miner.Mine
                                key
                                blocks
                                blockTransactions
                                time
                                threshold //18446744073709551615UL
                                1UL
                        match newBlock with
                        | Some block -> 
                            let count = ((blocks |> List.length) + 1)
                            printfn "Mined block #%i with nonce=%A after %A" count block.nonce (DateTime.Now.Subtract(time))
                            match block.transactions with
                            | _ :: rest -> rest |> List.iter (fun tx -> printfn "Transaction %s" (describe tx))
                            | _ -> ()
                            
                            blocks <- block :: blocks
                            blockPersistance block count
                            Async.Start <| Notifications.notify count
                        | _ -> 
                            lock mempoolLocker (fun () ->
                                mempool <- blockTransactions @ mempool
                            )
                            ()
                    )
            }

        member _.Stop =
            lock monitor (fun () -> continueLooping <- true)

        member _.InitBlocks initialBlocks =
            lock monitor (fun () -> blocks <- initialBlocks)

        member _.TryPay pubkey amount =
            Wallet.tryPay blocks pubkey amount

        member private _.PayInner transaction =
            lock mempoolLocker 
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
            let filterChange (o, t) =
                t.inputs |> List.exists (fun i -> Convert.ToBase64String(hash i.pubKey) = Convert.ToBase64String(o.pubKeyHash)) |> not

            let alloutputs =
                blocks
                |> List.map 
                    (fun block -> 
                        block.transactions 
                        |> List.map (fun t -> t.outputs |> List.map (fun o -> o, t)) 
                        |> List.concat)
                |> List.concat

            let myoutputs =
                alloutputs     
                |> List.filter filterChange
                |> List.filter (fun (output, _) -> output.pubKeyHash = hash pubkey)

            let confirmed =
                myoutputs                
                |> List.map 
                    (fun (output, tx) -> 
                        let tp = if tx.inputs |> List.isEmpty then Mined else Incoming
                        { confirmed = true; 
                          amount = output.value; 
                          address = Convert.ToBase64String(output.pubKeyHash); 
                          type_ = tp;
                          time = tx.time }
                    )
                    
            let unconfirmed = 
                mempool                
                |> List.map (fun t -> t.outputs |> List.map (fun o -> o, t))
                |> List.concat
                |> List.filter filterChange                
                |> List.filter (fun (output, _) -> output.pubKeyHash = hash pubkey)
                |> List.map (fun (output, tx) -> 
                    { confirmed = false; 
                      amount = output.value; 
                      address = Convert.ToBase64String(output.pubKeyHash); 
                      type_ = Incoming;
                      time = tx.time })

            confirmed @ unconfirmed

        member _.GetOutgoingTransactions pubkey =
            let indexedInputs = 
                blocks
                |> List.rev
                |> List.mapi 
                    (fun blockIndex block -> 
                        block.transactions 
                        |> List.map (fun t -> t.inputs |> List.map (fun i -> i, t)) 
                        |> List.concat 
                        |> List.map (fun (input, t) -> input, blockIndex, t))
                |> List.concat

            let myinputs =
                indexedInputs
                |> List.filter (fun (input, _, _) -> input.pubKey = pubkey)

            let confirmed =
                myinputs
                |> List.choose 
                    (fun (input, blockIndex, tx) -> 
                        let amountWithoutChange output =
                            let change =
                                tx.outputs 
                                |> List.filter (fun o -> Convert.ToBase64String(o.pubKeyHash) = Convert.ToBase64String(output.pubKeyHash))
                                |> List.sumBy (fun o -> o.value)
                            output.value - change                       
                                
                        input 
                        |> findOutputByInput (blocks |> List.take blockIndex)
                        |> Option.map (fun output -> 
                            { confirmed = true; 
                              amount = amountWithoutChange output; 
                              address = Convert.ToBase64String(output.pubKeyHash); 
                              type_ = Outgoing;
                              time = tx.time }))
                    
            let unconfirmed = 
                mempool
                |> List.filter (fun t -> t.inputs |> List.exists (fun i -> i.pubKey = pubkey))
                |> List.map 
                    (fun t -> 
                        t.outputs 
                        |> List.filter (fun o -> o.pubKeyHash <> hash pubkey)
                        |> List.map (fun o -> o, t))
                |> List.concat
                |> List.map (fun (output, tx) -> 
                    { confirmed = false; 
                      amount = output.value; 
                      address = Convert.ToBase64String(output.pubKeyHash); 
                      type_ = Outgoing;
                      time = tx.time })

            confirmed @ unconfirmed

        member this.GetTransactions pubkey =
            this.GetIncomingTransactions pubkey @ this.GetOutgoingTransactions pubkey