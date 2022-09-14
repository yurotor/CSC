namespace CSC.IO

open System.IO
open Blockchain
open Wallet

[<AutoOpen>]
module IO =
    
    let private DATA_DIR = "Data"

    let readBlock path i = 
        let path = Path.Combine(path, DATA_DIR, (sprintf "%i.dat" i))
        if File.Exists(path) then
            File.ReadAllText(path) |> Some
        else
            None

    let writeBlock path i b =
        let path = Path.Combine(path, DATA_DIR, (sprintf "%i.dat" i))
        File.WriteAllText(path, b)

    let saveBlock path block i =
        block
        |> Serializer.serialize 
        |> writeBlock path i

    let loadBlockchain path =
        let rec loadBlock blocks i =
            match readBlock path i with
            | Some stream -> 
                let block = Serializer.deserialize<Block> stream 
                loadBlock (block :: blocks) (i + 1)
            | _ -> blocks
        loadBlock [] 1

    let loadWallet path =
        load (fun name -> System.IO.File.ReadAllText(name)) Serializer.deserialize<Wallet.Wallet> path

    let saveWallet path wallet =
        Wallet.save 
            (fun w -> async { File.WriteAllText(Path.Combine(path, wallet.name), w) }) 
            Serializer.serialize 
            wallet