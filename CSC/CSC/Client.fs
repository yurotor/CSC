module Client

open Blockchain

    type Client() =
        
        static member blockchain: ResizeArray<Block> = new ResizeArray<Block>()

        

        static member run =
            while true do
                printfn "hi"

            ()


                    

