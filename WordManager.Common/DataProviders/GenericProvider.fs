namespace WordManager.Common.DataProvider 

module GenericProvider =

    type GenericProvider<'T, 'U>(insertStrategy : 'U -> string,
                                 updateStrategy : 'U -> string,
                                 retrieveStrategy : 'U -> 'T) =
        member this.insert = insertStrategy
        member this.update = updateStrategy
        member this.retrieve = retrieveStrategy

