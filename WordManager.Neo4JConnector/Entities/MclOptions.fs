namespace Grapher.Entities
module MclOptions=


    type MclOptions = private {expansionFactor : int; inflationFactor : int}

    let get (option: MclOptions) = (option.expansionFactor, option.inflationFactor)

    let create(expansionFactor, inflationFactor) =
        if expansionFactor < 1 || inflationFactor < 1 then
            None
        else
            Some({expansionFactor = expansionFactor; inflationFactor = inflationFactor})