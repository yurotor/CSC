namespace CSC.Model

module Miner =
    //let mutable log: string -> unit = fun _ -> ()

    type Miner(log) =
        let mutable monitor = new System.Object()
        let mutable stopper = false

        member _.Mine key blocks transactions time threshold initNonce = 
            let utime = unixTime time
            let tx = Wallet.createCoinbaseTransaction 100UL utime key
            let prevBlock = blocks |> List.tryHead
            let mutable block: Blockchain.Block option = None
            let mutable nonce = initNonce

            stopper <- false
            while stopper <> true do
                lock monitor (fun () ->
                    match Blockchain.tryCreateBlock prevBlock (tx :: transactions) utime threshold nonce with
                    | Some b -> 
                        log <| sprintf "Found block number %i nonce %A" ((blocks |> List.length) + 1) nonce
                        block <- Some b 
                        stopper <- true
                    | _ -> 
                        nonce <- Blockchain.nextNonce nonce
                )

            block

        member _.Stop () =
            lock monitor (fun () -> stopper <- true)

    let defaultMiner () = Miner(fun _ -> ())
    
    //let mine key blocks transactions time threshold initNonce =
    //    let utime = unixTime time
    //    let tx = Wallet.createCoinbaseTransaction 100UL utime key
    //    let prevBlock = blocks |> List.tryHead
    //    let mutable block: Blockchain.Block option = None
    //    let mutable nonce = initNonce

    //    stopper <- false
    //    while stopper <> true do
    //        lock monitor (fun () ->
    //            match Blockchain.tryCreateBlock prevBlock (tx :: transactions) utime threshold nonce with
    //            | Some b -> 
    //                //log <| sprintf "Found block number %i nonce %A" ((blocks |> List.length) + 1) nonce
    //                block <- Some b 
    //                stopper <- true
    //            | _ -> 
    //                nonce <- Blockchain.nextNonce nonce
    //        )

    //    block


    //let stop =
    //    lock monitor (fun () -> stopper <- true)

    //let setLogger logger =
    //    log <- logger