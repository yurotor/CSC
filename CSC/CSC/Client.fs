namespace CSC

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
        Wallet.tryPay blocks pubkey amount

    let getBalance pubkey =
        let utxos = getUTXOSet blocks
        let myutxos = 
            utxos
            |> List.filter (fun utxo -> utxo.output.pubKeyHash = hash pubkey)
        myutxos |> List.sumBy (fun u -> u.output.value)
        //blocks
        //|> getUTXOSet 
        //|> List.filter (fun utxo -> utxo.output.pubKeyHash = hash pubkey)
        //|> List.sumBy (fun u -> u.output.value) 

    let pay transaction =
        lock monitor 
            (fun () -> 
                mempool <- transaction :: mempool
                let sum = transaction.outputs |> List.sumBy (fun o -> o.value) 
                printfn "New transaction entered the mempool - %i $" sum
            )

    let getIncomingTransactions pubkey =
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

    let getOutgoingTransactions pubkey =
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
                    |> Option.map (fun output -> { confirmed = true; amount = output.value; address = Convert.ToBase64String(output.pubKeyHash); type_ = Incoming }))
                
        let unconfirmed = 
            mempool
            |> List.map (fun t -> t.outputs)
            |> List.concat
            |> List.filter (fun output -> output.pubKeyHash = hash pubkey)
            |> List.map (fun output -> { confirmed = false; amount = output.value; address = Convert.ToBase64String(output.pubKeyHash); type_ = Incoming })

        confirmed @ unconfirmed

    let getTransactions pubkey =
        getIncomingTransactions pubkey @ getOutgoingTransactions pubkey

                    

