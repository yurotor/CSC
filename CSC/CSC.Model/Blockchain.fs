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
        time: Time
    }

    let outputHash output =
        Array.concat [BitConverter.GetBytes(output.value); output.pubKeyHash] |> hash

    let toBytes (transaction: Transaction) =
        let inputs = 
            if transaction.inputs |> List.length > 0 then
                transaction.inputs
                |> List.map (fun i -> 
                    Array.concat [i.prevTxId; BitConverter.GetBytes(i.prevTxIndex); i.pubKey; i.signature])
                |> Array.concat
            else
                Array.zeroCreate 32
                
        let outputs =
            transaction.outputs
            |> List.map (fun o -> Array.concat [o.pubKeyHash; BitConverter.GetBytes(o.value)])
            |> Array.concat

        Array.concat [ BitConverter.GetBytes(transaction.time); inputs; outputs ]

    let toSign prevTxId (index: int) =
        Array.concat [prevTxId; BitConverter.GetBytes(index)]

    let blockHeaderHash block =
        Array.concat 
            [ block.prevBlockHeaderHash;
              block.content;
              BitConverter.GetBytes(block.time);
              BitConverter.GetBytes(block.nonce);
            ] 
            |> hash

    let getBlockContent =
        List.map toBytes
        >> Array.concat
        >> CompressedSensing.calculate CompressedSensing.SIZE
        >> CompressedSensing.matrixToBytes
        >> hash

    let tryCreateBlock prevBlock transactions time threshold nonce =
        let content = transactions |> getBlockContent

        let prevBlockHeaderHash = 
            prevBlock |> Option.map blockHeaderHash |> Option.defaultValue (Array.zeroCreate 32)

        let block = 
            { prevBlockHeaderHash = prevBlockHeaderHash; content = content; time = time; nonce = nonce; transactions = transactions }

        let num = block |> blockHeaderHash |> hashToNumber
        if num < threshold then
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
        |> List.tryFind (outputHash >> (=) input.prevTxId) 

    let validateTransaction blocks transaction =
        let inputsValidation =
            transaction.inputs
            |> List.fold 
                (fun state input ->
                    result {
                        let! output = 
                            blocks 
                            |> List.map (fun b -> b.transactions)
                            |> List.concat
                            |> List.choose 
                                (fun t -> 
                                    t.outputs 
                                    |> List.tryItem input.prevTxIndex 
                                    |> Option.tee (outputHash >> (=) input.prevTxId)
                                )
                            |> List.tryHead
                            |> Result.ofOption { error = "Can't find input source"; type_ = InputSourceMissing }

                        let! _ =
                            output.pubKeyHash = hash input.pubKey 
                            |> Result.ofBool { error = "Public key hash mismatch"; type_ = PublicKeyMismatch }

                        let! _ =
                            verifySig input.signature input.pubKey (toSign input.prevTxId input.prevTxIndex)
                            |> not
                            |> Result.ofBool { error = "Singature mismatch"; type_ = SignatureMismatch }

                        return ()
                    }
                    |> ValidationResult.chain state
                )
                Valid

        if transaction.inputs |> List.length > 0 then
            transaction.inputs
                |> List.fold
                    (fun sum input ->
                        let v = 
                            blocks 
                            |> List.map (fun b -> b.transactions)
                            |> List.concat
                            |> List.choose 
                                (fun t -> 
                                    t.outputs 
                                    |> List.tryItem input.prevTxIndex 
                                    |> Option.tee (outputHash >> (=) input.prevTxId)
                                )
                            |> List.tryHead
                            |> Option.map (fun o -> o.value)
                            |> Option.defaultValue 0UL
                        
                        sum + v
                    )
                    0UL
                |> (fun sum -> 
                        let outputsSum =
                            transaction.outputs |> List.sumBy (fun o -> o.value)
                        if sum = outputsSum then Ok ()
                        else Error { error = "Transaction inputs and output sums are not equal"; type_ = TransactionAmountInvalid }
                    )
                |> ValidationResult.chain inputsValidation
        else 
            inputsValidation

    let validateBlockHeader threshold prevblock block =
        prevblock
        |> Option.map (blockHeaderHash >> (=) block.prevBlockHeaderHash)
        |> Option.defaultValue true
        |> Result.ofBool { error = "Block header hash mismatch"; type_ = BlockHeaderHashMismatch }
        |> ValidationResult.ofResult
        |> ValidationResult.concat
            (block.transactions |> getBlockContent = block.content 
            |> Result.ofBool { error = "Block content mismatch"; type_ = BlockContentMismatch }
            |> ValidationResult.ofResult)
        |> ValidationResult.concat  
            (block |> blockHeaderHash |> hashToNumber < threshold
            |> Result.ofBool { error = "Invalid nonce"; type_ = InvalidNonce }
            |> ValidationResult.ofResult)

    let validateBlockchain threshold blocks =
        let rec validate prev rest valres =
            match rest with
            | head :: tail -> 
                valres
                |> ValidationResult.concat
                    (validateBlockHeader threshold prev head)
                |> ValidationResult.concat
                    (head.transactions 
                    |> List.fold 
                        (fun state t -> validateTransaction blocks t |> ValidationResult.concat state) 
                        Valid
                    )
                |> validate (Some head) tail
            | _ -> valres
            
        validate None blocks Valid

    let nextNonce nonce = 
        nonce + 1UL

    let describe tx =
        tx.outputs
        |> List.map (fun o -> sprintf "%A to %i" o.value (BitConverter.ToUInt32(o.pubKeyHash)))
        |> String.concat " "

    let describeUserTransaction tx =
        sprintf "%b;%A;%i;%s;%i" tx.confirmed tx.type_ tx.amount tx.address tx.time