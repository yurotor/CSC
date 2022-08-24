module Miner

    let mutable monitor = new System.Object()
    let mutable stopper = false
    
    let mine key blocks time threshold nonce =
        let rec inner key blocks time threshold nonce =
            lock monitor (fun () ->
                if stopper then None
                else
                    let utime = unixTime time
                    let tx = Wallet.createCoinbaseTransaction 100UL utime key
                    let prevBlock = blocks |> List.tryHead
                    match Blockchain.tryCreateBlock prevBlock [ tx ] utime threshold nonce with
                    | Some block -> Some block 
                    | _ -> inner key blocks time threshold (Blockchain.nextNonce nonce)
            )
        stopper <- false
        inner key blocks time threshold nonce

    let stop =
        lock monitor (fun () -> stopper <- true)