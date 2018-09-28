namespace WordManager.Framework.Tools
module Seq = 
    

    let tryItem (index: int) (collection: 'a seq) =
        if (Seq.length(collection) - 1) < index && index >= 0 then
            Option.None
        else
            (Seq.nth index collection) |> Option.Some


    let tryHead collection = tryItem 0 collection  
        
    let count(predicate : 't -> bool)(collection : 't seq) = 
        let rec f count (col: 't list) =
            match col with
            | h :: t when predicate h ->
                f (count + 1) t
            | _ :: t -> 
                f count t
            | _ -> count
        
        f 0 (collection |> List.ofSeq)
