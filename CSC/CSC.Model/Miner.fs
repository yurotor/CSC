﻿module Miner

    let mutable monitor = new System.Object()
    let mutable stopper = false
    let mutable log: string -> unit = fun _ -> ()
    
    let mine key blocks time threshold initNonce =
        let utime = unixTime time
        let tx = Wallet.createCoinbaseTransaction 100UL utime key
        let prevBlock = blocks |> List.tryHead
        let mutable block: Blockchain.Block option = None
        let mutable nonce = initNonce

        stopper <- false
        while stopper <> true do
            lock monitor (fun () ->
                match Blockchain.tryCreateBlock prevBlock [ tx ] utime threshold nonce with
                | Some b -> 
                    //log <| sprintf "Found block number %i nonce %A" ((blocks |> List.length) + 1) nonce
                    block <- Some b 
                    stopper <- true
                | _ -> 
                    nonce <- Blockchain.nextNonce nonce
                    //inner key blocks time threshold (Blockchain.nextNonce nonce)
                    
            )

        block

        //let rec inner key blocks time threshold nonce =
        //    lock monitor (fun () ->
        //        if stopper then None
        //        else
        //            //log <| sprintf "Mining nonce %A" nonce
        //            match Blockchain.tryCreateBlock prevBlock [ tx ] utime threshold nonce with
        //            | Some block -> 
        //                log <| sprintf "Found block number %i nonce %A" ((blocks |> List.length) + 1) nonce
        //                Some block 
        //            | _ -> inner key blocks time threshold (Blockchain.nextNonce nonce)
        //    )
        
        //inner key blocks time threshold nonce

    let stop =
        lock monitor (fun () -> stopper <- true)

    let setLogger logger =
        log <- logger