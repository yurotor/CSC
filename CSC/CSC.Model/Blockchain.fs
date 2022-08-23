module Blockchain

open Common.Crypto

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

    let toSign prevTxId index =
        Array.concat [prevTxId; bytesOf (index.ToString())]

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

    let validateTransaction txid blocks transaction =
        transaction.inputs
        |> List.forall 
            (fun input ->
                blocks 
                |> List.map (fun b -> b.transactions)
                |> List.concat
                |> List.tryFind (fun t -> txid t = input.prevTxId)
                |> Option.bind (fun t -> t.outputs |> List.tryItem input.prevTxIndex)
                |> Option.map 
                    (fun o -> 
                        o.pubKeyHash = hash input.pubKey && 
                        verifySig input.signature input.pubKey (toSign input.prevTxId input.prevTxIndex)
                    )
                |> Option.defaultValue false
            )

    let validateBlockHeader txid threshold prevblock block =
        prevblock
        |> Option.map (blockHeaderHash >> (=) block.prevBlockHeaderHash)
        |> Option.defaultValue true
        &&
        block.transactions |> List.map txid |> Array.concat = block.content 
        &&
        block |> blockHeaderHash |> leadingZeros >= threshold

    let validateBlockchain txid threshold blocks =
        let rec validate prev rest =
            match rest with
            | head :: tail -> 
                if validateBlockHeader txid threshold prev head &&
                    head.transactions |> List.forall (validateTransaction txid blocks)
                    then
                    validate (Some head) tail
                else
                    false
            | _ -> true
            
        validate None blocks

    let nextNonce nonce = 
        nonce + 1UL