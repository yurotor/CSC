module Blockchain

open Common.Crypto
open System

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
        output: TxOutput
        index: int
    }

    type UserTransactionType =
        | Incoming
        | Outgoing
        | Mined

    type UserTransaction = {
        confirmed: bool
        type_: UserTransactionType
        amount: uint64
        address: string
    }

    let outputHash output =
        Array.concat [bytesOf (output.value.ToString()); output.pubKeyHash] |> hash

    let toBytes transaction =
        [
        transaction.inputs
        |> List.map (fun i -> Array.concat [i.prevTxId; bytesOf <| i.prevTxIndex.ToString(); i.pubKey; i.signature])
        |> Array.concat;
        transaction.outputs
        |> List.map (fun o -> Array.concat [o.pubKeyHash; bytesOf <|  o.value.ToString()])
        |> Array.concat
        ]
        |> Array.concat

    let toSign prevTxId index =
        Array.concat [prevTxId; bytesOf (index.ToString())]

    let blockHeaderHash block =
        Array.concat 
            [ block.prevBlockHeaderHash;
              block.content; 
              bytesOf (block.time.ToString()); 
              bytesOf (block.nonce.ToString()) ] 
            |> hash

    let tryCreateBlock prevBlock transactions time threshold nonce =
        let content =
            transactions
            |> List.map toBytes
            |> Array.concat

        let prevBlockHeaderHash = 
            prevBlock |> Option.map blockHeaderHash |> Option.defaultValue (Array.zeroCreate 32)

        let block = 
            { prevBlockHeaderHash = prevBlockHeaderHash; content = content; time = time; nonce = nonce; transactions = transactions }

        if block |> blockHeaderHash |> hashToNumber < threshold then
            Some block
        else
            None

    let getUTXOSet (blocks: Block list) : UTXO list =
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
                                    match u_ |> List.tryFindIndex (fun i -> outputHash i.output = input.prevTxId) with
                                    | Some index -> u_ |> List.removeAt index
                                    | _ -> u_
                                )
                                u

                        t.outputs
                            |> List.fold
                                (fun (u_, index) o ->
                                    ({ transaction = t; output = o; index = index } :: u_, index + 1)
                                )
                                (markSpentInputs, 0)
                            |> fst
                    )
                    utxos                
            )
            []

    let findOutputByInput blocks input =
        blocks
        |> List.map (fun block -> block.transactions |> List.map (fun t -> t.outputs) |> List.concat)
        |> List.concat
        |> List.tryFind (fun o -> outputHash o = input.prevTxId)

    let validateTransaction blocks transaction =
        transaction.inputs
        |> List.fold 
            (fun state input ->
                result {
                    let! t = 
                        blocks 
                        |> List.map (fun b -> b.transactions)
                        |> List.concat
                        |> List.tryFind (fun t -> toBytes t = input.prevTxId)
                        |> Result.ofOption "Can't find transaction"

                    let! o = 
                        t.outputs 
                        |> List.tryItem input.prevTxIndex
                        |> Result.ofOption "Can't find prevTxIndex"

                    let! _ =
                        o.pubKeyHash = hash input.pubKey
                        |> Result.ofBool "Public key hash mismatch"

                    let! _ =
                        verifySig input.signature input.pubKey (toSign input.prevTxId input.prevTxIndex)
                        |> Result.ofBool "Singature mismatch"

                    return t
                }
                |> ValidationResult.chain state
            )
            Valid

    let validateBlockHeader threshold prevblock block =
        prevblock
        |> Option.map (blockHeaderHash >> (=) block.prevBlockHeaderHash)
        |> Option.defaultValue true
        |> Result.ofBool "Block header hash mismatch"
        |> ValidationResult.ofResult
        |> ValidationResult.concat
            (block.transactions |> List.map toBytes |> Array.concat = block.content 
            |> Result.ofBool "Block content mismatch"
            |> ValidationResult.ofResult)
        |> ValidationResult.concat  
            (block |> blockHeaderHash |> hashToNumber < threshold
            |> Result.ofBool "Invalid nonce"
            |> ValidationResult.ofResult)

    let validateBlockchain threshold blocks =
        let rec validate prev rest =
            match rest with
            | head :: tail -> 
                validateBlockHeader threshold prev head 
                |> ValidationResult.concat
                    (head.transactions 
                    |> List.fold 
                        (fun state t -> validateTransaction blocks t |> ValidationResult.concat state) 
                        Valid
                    )
                |> ValidationResult.bind (validate (Some head) tail)
            | _ -> Valid
            
        validate None blocks

    let nextNonce nonce = 
        nonce + 1UL

    let describe tx =
        tx.outputs
        |> List.map (fun o -> sprintf "%A to %i" o.value (BitConverter.ToUInt32(o.pubKeyHash)))
        |> String.concat " "