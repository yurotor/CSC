namespace CSC

module Notifications =
    let mutable listeners: (int -> unit) list = []
    let mutable monitor = new System.Object()
    let event = new System.Threading.AutoResetEvent(false)

    let internal notify num =
        async {
            lock monitor (fun () ->
                event.Set() |> ignore
                listeners
                |> List.iter (fun f -> f num)               
            )
        }
    
    let addListener listener =
        lock monitor (fun () -> listeners <- listener :: listeners)

    let wait () =
        event.WaitOne() |> ignore

    let waitFor times =
        if times < 1 then ()
        else
            let mutable counter = times
            while (counter > 0)
                do
                    wait ()
                    counter <- counter - 1

