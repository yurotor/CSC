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

    let leadingZeros (hash: Hash) =
        let rec count i x =
            match x with
            | head :: tail when head = 0uy -> count (i + 1) tail
            | _ -> i
        count 0 (hash |> List.ofArray)

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
            Array.concat [prevTxId; bytesOf (index.ToString())]
        
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
            
        
        

