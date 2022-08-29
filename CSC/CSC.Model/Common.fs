[<AutoOpen>]
module Common
open System.Security.Cryptography
open System
open System.Text
open NBitcoin.Secp256k1

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

    //let leadingZeros (hash: Hash) =
    //    let rec count i x =
    //        match x with
    //        | head :: tail when head = 0uy -> count (i + 1) tail
    //        | _ -> i
    //    count 0 (hash |> List.ofArray)

    let hashToNumber hash =
         BitConverter.ToUInt64 (ReadOnlySpan<byte> hash)

    type ResultBuilder() =
           member __.Return(x) = Ok x
           member __.Bind(m, f) = Result.bind f m
           member __.Zero() = None
           member __.Delay(f: unit -> _) = f
           member __.Run(f) = f()
           member __.ReturnFrom(m: Result<_, _>) = m
           member __.TryWith(m, h) =
               try __.ReturnFrom(m)
               with e -> h e

    let result = ResultBuilder()

    [<AutoOpen>]
    module Result =
        let ofOption onMissing v =
            match v with 
            | Some x -> Ok x
            | None -> Error onMissing 

        let ofBool onFalse v = 
            match v with 
            | true -> Ok true
            | false -> Error onFalse

    type ValidationResult =
        | Valid
        | Invalid of string list

    module ValidationResult =
        let ofResult res =
            match res with
            | Ok _ -> Valid
            | Error e -> Invalid [e]

        let chain vr res =
            match vr, res with
            | Valid, Ok _ -> Valid
            | Valid, Error e -> Invalid [e]
            | Invalid l, Ok _ -> Invalid l
            | Invalid l, Error e -> Invalid <| e :: l

        let concat vr1 vr2 =
            match vr1, vr2 with
            | Valid, Valid -> Valid
            | Valid, Invalid l -> Invalid l
            | Invalid l, Valid -> Invalid l
            | Invalid l1, Invalid l2 -> Invalid <| l1 @ l2

        let bind f vr =
            match vr with
            | Valid -> vr
            | Invalid l -> Invalid l

    module Crypto =

        let createPrivateKeyBytes =        
            Key.generate

        let createPrivateKey privKey =
            ECPrivKey.Create (ReadOnlySpan<byte> privKey)

        let createPubKey privKey =
            let pr = createPrivateKey privKey
            pr.CreatePubKey ()     

        let createPubKeyBytes privKey =
            let pubkey = createPubKey privKey
            let mutable buffer: Span<byte> = Span<byte> (Array.zeroCreate 33)
            let mutable len = 0
            pubkey.WriteToSpan (true, buffer, &len)
            buffer.ToArray ()

        let toSign prevTxId index =
            Array.concat [prevTxId; bytesOf (index.ToString())] |> hash
        
        let sign key data =
            let prKey = createPrivateKey key
            let mutable signature: SecpECDSASignature = null
            if prKey.TrySignECDSA(ReadOnlySpan<byte> data, &signature) then
                let mutable buffer: Span<byte> = Span<byte> (Array.zeroCreate 64)
                signature.WriteCompactToSpan(buffer)
                Some <| buffer.ToArray ()
            else
                None

        let verifySig ``sig`` pubkey data =    
            let mutable signature: SecpECDSASignature = null
            let mutable publicKey: ECPubKey = null
            let mutable compressed: bool = true
            match 
                SecpECDSASignature.TryCreateFromCompact(ReadOnlySpan<byte> ``sig``, &signature),
                ECPubKey.TryCreate(ReadOnlySpan<byte> pubkey, null, &compressed, &publicKey)
            with
            | true, true -> publicKey.SigVerify(signature, ReadOnlySpan<byte> data)
            | _ -> false
            
        
        

