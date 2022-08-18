
open System
open CSC.IO

let key = Wallet.createPrivateKeyBytes 
let wallet = {Wallet.name="ukeselman";Wallet.keys=[key]}

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

let blocks = loadBlockchain
let newBlock =
    Miner.mine 
        Serializer.txid
        key
        blocks
        DateTime.Now
        1
        1UL

match newBlock with
| Some block -> saveBlock block ((blocks |> List.length) + 1)
| _ -> ()
    //Blockchain.load 
    //    (fun i ->
    //        let path = Path.Combine(Directory.GetCurrentDirectory(), "Blockchain", (sprintf "%i.dat" i))
    //        if File.Exists(path) then
    //            File.ReadAllText(path) |> Some
    //        else
    //            None
    //    )

System.Console.ReadLine() |> ignore