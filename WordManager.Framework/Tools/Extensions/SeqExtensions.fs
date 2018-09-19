namespace WordManager.Framework.Tools
module Seq = 
    

    let tryItem (index: int) (collection: 'a seq) =
        if (Seq.length(collection) - 1) < index && index >= 0 then
            Option.None
        else
            (Seq.nth index collection) |> Option.Some


    let tryHead collection = tryItem 0 collection  
        
        

   

