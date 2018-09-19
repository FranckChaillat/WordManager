namespace WordManager.Common.DataProvider 

module GenericProvider =

    type GenericProvider<'T, 'U>(insertStrategy : string -> int option,
                                   updateStrategy : string -> int option,
                                   retrieveStrategy : string -> 'T,
                                   mappingStrategy : 'U -> 'T) =
        member this.insert = insertStrategy
        member this.update = updateStrategy
        member this.retrieve = retrieveStrategy
        member this.mapper = mappingStrategy

