namespace CSC

open System
open Common.Crypto
open Blockchain

module Expirement =
    
    let run txcount =
        //generate n transactions
        let time = unixTime DateTime.Now
        let key = createPrivateKeyBytes
        let txs = [1..txcount] |> List.map (fun i -> Wallet.createCoinbaseTransaction (uint64(i)) time key)
        let bytes = Serializer.serialize txs |> String.length
        //hash them using CS and compute avg time
        let avg =
            [1..100]
            |> List.map
                (fun _ ->
                    let start = DateTime.Now
                    txs |> getBlockContent |> ignore
                    let span = DateTime.Now.Subtract(start)
                    span.TotalMilliseconds
                )
            |> List.average
        
        //printfn "Compressed sensing: Average time is %A for %i transactions" avg txcount
    
        //hash them using Merkle tree and compute avg time
        let input = txs |> List.map (toBytes >> hash)
        let rec compute (list: byte array list) =
            let res =
                list
                |> List.chunkBySize 2
                |> List.map (Array.concat >> hash)
                |> function
                    | [x] -> x
                    | (_ :: _ :: _) as list -> compute list
                    | _ -> [||]
            
            res
    
        
        let avg2 =
            [1..100]
            |> List.map
                (fun _ ->
                    let start = DateTime.Now
                    let _ = compute input
                    let span = DateTime.Now.Subtract(start)
                    span.TotalMilliseconds
                )
            |> List.average
    
        //printfn "Merkle tree: Average time is %A for %i transactions" avg txcount

        printfn "%i,%i,%A,%A;" txcount bytes avg avg2
    
        ()
    

