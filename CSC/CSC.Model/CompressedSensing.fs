[<AutoOpen>]
module CompressedSensing
    
    open FSharp.Stats
    open System

    let SIZE = 32
    
    let createVector (bytes: byte array) =
        let v = rowvec (bytes |> Array.map float)
        //printfn "v=%s" (v.ToString ())
        bytes

    let createMatrix (m: int) (bytes: byte array) =
        let seeds = bytes |> Array.map int

        let cols =
            seeds
            |> Array.map 
                (fun i -> 
                    let rand = new Random.RandBasic(i)
                    [1..m]
                    |> List.map (fun _ -> float <| rand.rnd.Next())
                )
            |> List.ofArray

        let matrix = Matrix.ofJaggedColList cols

        //printf "Matrix = %s" (matrix.ToString())

        matrix

    let calculate (m: int) (bytes: byte array) =
        let matrix = createMatrix m bytes
        let vector = Matrix.ofJaggedColList <| [bytes |> Array.map float |> List.ofArray]
        matrix * vector
        
    let matrixToBytes =
        Matrix.toJaggedArray
        >> Array.concat
        >> Array.map (fun f -> BitConverter.GetBytes(f))
        >> Array.concat