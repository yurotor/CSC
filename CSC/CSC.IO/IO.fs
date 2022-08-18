namespace CSC.IO

open System.IO
open Blockchain

[<AutoOpen>]
module IO =
    
    let readBlock i = 
        let path = Path.Combine(Directory.GetCurrentDirectory(), "Blockchain", (sprintf "%i.dat" i))
        if File.Exists(path) then
            File.ReadAllText(path) |> Some
        else
            None

    let writeBlock i b =
        let path = Path.Combine(Directory.GetCurrentDirectory(), "Blockchain", (sprintf "%i.dat" i))
        File.WriteAllText(path, b)

    let saveBlock block i =
        block
        |> Serializer.serialize 
        |> writeBlock i

    let loadBlockchain =
        let rec loadBlock blocks i =
            match readBlock i with
            | Some stream -> 
                let block = Serializer.deserialize<Block> stream 
                loadBlock (block :: blocks) (i + 1)
            | _ -> blocks
        loadBlock [] 0