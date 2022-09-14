namespace CSC.Model

open System

module Miner =
    type Miner() =
        let mutable monitor = Object()
        let mutable stopper = false

        member _.Mine key blocks transactions time threshold initNonce = 
            let utime = unixTime time
            let tx = Wallet.createCoinbaseTransaction 100UL utime key
            let prevBlock = blocks |> List.tryHead
            let mutable block: Blockchain.Block option = None
            let mutable count = 1
            let mutable rand = Random()
            let mutable nonce = uint64(rand.NextInt64())

            stopper <- false
            while stopper <> true do
                lock monitor (fun () ->
                    match Blockchain.tryCreateBlock prevBlock (tx :: transactions) utime threshold nonce with
                    | Some b -> 
                        block <- Some b 
                        stopper <- true
                        printfn "Mined after %i trials" count
                        count <- 1
                    | _ -> 
                        nonce <- uint64(rand.NextInt64())
                        count <- count + 1
                )

            block

        member _.Stop () =
            lock monitor (fun () -> stopper <- true)

    let defaultMiner () = Miner()
    