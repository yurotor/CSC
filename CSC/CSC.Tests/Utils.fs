[<AutoOpen>]
module Utils
open System
open CSC

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

    let defaultServer _ = Server(fun _ _ -> ())

