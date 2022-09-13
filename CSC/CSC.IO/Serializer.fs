module Serializer

    open Newtonsoft.Json
    open Blockchain

    let serialize i =
        JsonConvert.SerializeObject(i)

    let deserialize<'t> i =
        JsonConvert.DeserializeObject<'t>(i)

    //let txid : (Transaction -> Hash) =
    //    serialize >> bytesOf >> hash