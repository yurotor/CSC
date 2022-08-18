[<AutoOpen>]
module Common
open System.Security.Cryptography
open System
open System.Text

    type Time = int64
    type Hash = byte array
    type Key = byte array
    type Signature = byte array

    module Key =
        let generate =
            let rand = System.Random()
            let buffer = Array.create<byte> 32 0uy
            rand.NextBytes(buffer)
            buffer

    let hash (x: byte array) : byte array =
        use sha256 = SHA256.Create()
        sha256.ComputeHash(x)

    let unixTime (time: DateTime) : Time =
        DateTimeOffset(time).ToUnixTimeSeconds()

    let bytesOf (s: string) = 
        Encoding.ASCII.GetBytes(s)

    let leadingZeros (hash: Hash) =
        let rec count i x =
            match x with
            | head :: tail when head = 0uy -> count (i + 1) tail
            | _ -> i
        count 0 (hash |> List.ofArray)
        
        

