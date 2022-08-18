module Blockchain

    type TxInput = {
        prevTxId: Hash
        prevTxIndex: int
        pubKey: Key
        signature: Signature
    }

    type TxOutput = {
        value: uint64
        pubKeyHash: Hash
    }

    type Transaction = {
        inputs: TxInput list
        outputs: TxOutput list
        time: Time
    }

    type Block = {
        prevBlockHeaderHash: Hash
        content: Hash
        time: Time
        nonce: uint64
        transactions: Transaction list
    }

    type UTXO = {
        transaction: Transaction
        index: int
    }

    let blockHeaderHash block =
        Array.concat 
            [ block.prevBlockHeaderHash;
              block.content; 
              bytesOf (block.time.ToString()); 
              bytesOf (block.nonce.ToString()) ] 
            |> hash

    let tryCreateBlock txid prevBlock transactions time threshold nonce =
        let content =
            transactions
            |> List.map txid
            |> Array.concat

        let prevBlockHeaderHash = 
            prevBlock |> Option.map blockHeaderHash |> Option.defaultValue (Array.zeroCreate 32)

        let block = 
            { prevBlockHeaderHash = prevBlockHeaderHash; content = content; time = time; nonce = nonce; transactions = transactions }

        if block |> blockHeaderHash |> leadingZeros >= threshold then
            Some block
        else
            None

    let getUTXOSet txid (blocks: Block list) : UTXO list =
        blocks
        |> List.fold 
            (fun utxos block ->
                block.transactions
                |> List.fold
                    (fun u t ->
                        let markSpentInputs =
                            t.inputs
                            |> List.fold
                                (fun u_ input ->
                                    match u_ |> List.tryFindIndex (fun i -> txid i.transaction = input.prevTxId) with
                                    | Some index -> u_ |> List.removeAt index
                                    | _ -> u_
                                )
                                u

                        t.outputs
                            |> List.fold
                                (fun (u_, index) _ ->
                                    ({ transaction = t; index = index } :: u_, index + 1)
                                )
                                (markSpentInputs, 0)
                            |> fst
                        
                    )
                    utxos                
            )
            []

    //let validateTransaction txid blocks transaction =
    //    transaction.inputs
    //    |> List.map 
    //        (fun input ->
    //            let output =
    //                blocks 
    //                |> List.map (fun b -> b.transactions)
    //                |> List.concat
    //                |> List.tryFind (fun t -> txid t = input.prevTxId)
    //                |> Option.bind (fun t -> t.outputs |> List.tryItem input.prevTxIndex)
    //            //match output with
    //            //| Some o -> o.pubKeyHash = hash input.pubKey
    //        )

    //let validateBlockchain blocks =
    //    blocks
    //    |> List.fold
    //        (fun v b ->
                
    //        )
    //        true

    let nextNonce nonce = 
        nonce + 1UL