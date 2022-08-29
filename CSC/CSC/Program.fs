
open System
open CSC.IO
open Crypto



//Wallet.save (fun w -> File.WriteAllText(wallet.name, w)) Serializer.serialize wallet

//let wallet2 = 
//    Wallet.load (fun name -> File.ReadAllText(name)) Serializer.deserialize<Wallet.Wallet> wallet.name



//let block = Blockchain.tryCreateBlock None [ tx ] time 1 1UL

//let rec mine blocks threshold nonce lim =
//    let time = unixTime DateTime.Now
//    let tx = Wallet.createCoinbaseTransaction 100UL time key
//    let prevBlock = blocks |> List.tryHead
//    match Blockchain.tryCreateBlock prevBlock [ tx ] time threshold nonce with
//    | Some block -> 
//        if lim > (blocks |> List.length) then mine (block :: blocks) threshold nonce lim
//        else (block :: blocks)
//    | _ -> mine blocks threshold (Blockchain.nextNonce nonce) lim

//let blocks = mine [] 1 1UL 10

//blocks
//|> List.iteri
//    (fun i b ->
//        Blockchain.save 
//            (fun s -> 
//                let path = Path.Combine(Directory.GetCurrentDirectory(), "Blockchain", (sprintf "%i.dat" i))
//                File.WriteAllText(path, s)
//            ) 
//            b
//    )

//let blocks = loadBlockchain
//let newBlock =
//    Miner.mine 
//        key
//        blocks
//        DateTime.Now
//        1
//        1UL

//match newBlock with
//| Some block -> saveBlock block ((blocks |> List.length) + 1)
//| _ -> ()
    //Blockchain.load 
    //    (fun i ->
    //        let path = Path.Combine(Directory.GetCurrentDirectory(), "Blockchain", (sprintf "%i.dat" i))
    //        if File.Exists(path) then
    //            File.ReadAllText(path) |> Some
    //        else
    //            None
    //    )

let mutable continueLooping = true
let start key blockchain =
    async {
        
        let mutable blocks = blockchain//loadBlockchain
        while continueLooping do
            //printfn "Start mining at %A" (DateTime.Now)
            let time = DateTime.Now
            let newBlock =
                Miner.mine 
                    key
                    blocks
                    time
                    4073709551615UL//18446744073709551615UL
                    1UL
            match newBlock with
            | Some block -> 
                let count = ((blocks |> List.length) + 1)
                printfn "Mined block #%i with nonce=%A after %A" count block.nonce (DateTime.Now.Subtract(time))
                printfn ""
                blocks <- block :: blocks
                saveBlock block count
            | _ -> ()
    }

let txcmd (cmd:string) =
    //tx from to amount
    let parts = cmd.Split(' ')
    let mutable amount: uint64 = 0UL
    if parts |> Array.length = 4 && parts.[0] = "tx" && UInt64.TryParse(parts.[3], &amount) then
        Some (parts.[1],parts.[2],amount)
    else
        None


[<EntryPoint>]
let main _ =
    Miner.setLogger (printfn "%s")
    let key = createPrivateKeyBytes
    Async.Start <| start key []

    while continueLooping do
        match System.Console.ReadLine() with
        | "q" -> 
            continueLooping <- false
            ()
        | cmd -> 
            ()

    0