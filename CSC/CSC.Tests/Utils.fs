[<AutoOpen>]
module Utils
open System
open CSC
open CSC.Model.Miner
open Blockchain
open Common.Crypto

    let private createBlockchainInner size key threshold initialNonce =
        let rec mine blocks threshold nonce lim =
            let time = unixTime DateTime.Now
            let tx = Wallet.createCoinbaseTransaction 100UL time key
            let prevBlock = blocks |> List.tryHead
            match Blockchain.tryCreateBlock prevBlock [ tx ] time threshold nonce with
            | Some block -> 
                if (lim - 1) > (blocks |> List.length) then mine (block :: blocks) threshold nonce lim
                else (block :: blocks)
            | _ -> mine blocks threshold (Blockchain.nextNonce nonce) lim

        mine [] threshold initialNonce size
        |> List.rev

    let createBlockchain size key =        
        createBlockchainInner size key 18446744073709551615UL 1UL

    let createBlockchainWithThreshold size key threshold =
        createBlockchainInner size key threshold 1UL

    let defaultThreshold = 18446744073709551615UL

    let defaultServer _ = Server((fun _ _ -> ()), defaultMiner (), defaultThreshold)

    let defaultServerWithThreshold threshold = Server((fun _ _ -> ()), defaultMiner (), threshold)

    let loadBlockchainOfSize i =
        "C:\\temp\\CSC"
        |> CSC.IO.IO.loadBlockchain 
        |> List.take i

    let buildTransactions blocks payerKey receiverKey amount = 
        let receiverPubkey = createPubKeyBytes receiverKey
        let payerPubKey = createPubKeyBytes payerKey
        match Wallet.tryPay blocks payerPubKey amount with
        | Ok (utxos, total) ->
            [ Wallet.buildTransaction (unixTime DateTime.Now) utxos total payerKey receiverPubkey amount ]
        | _ -> []

    let buildBlockchain transactionBuilder size key threshold =
        let miner = defaultMiner ()
        let mutable blocks: Block list = []
        while blocks |> List.length < size
            do
                let tx = transactionBuilder blocks
                match miner.Mine key blocks tx DateTime.Now threshold with
                | Some block ->
                    blocks <- blocks @ [block]
                | _ -> ()
        blocks
            