namespace WordManager.Common.DataProvider 

module GenericProvider =

    type GenericProvider<'A, 'B, 'OUT>(insertStrategy : 'A -> string,
                                       updateStrategy : 'A -> string,
                                       retrieveStrategy : 'B -> 'OUT option) =
        member this.insert = insertStrategy
        member this.update = updateStrategy
        member this.retrieve = retrieveStrategy

