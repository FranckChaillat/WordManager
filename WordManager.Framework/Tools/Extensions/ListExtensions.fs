namespace WordManager.Framework.Tools
module ListExtensions=
        
        let rec private replace acc predicate collection newElem = 
            match collection with
                | h :: t when predicate(h) -> replace (newElem :: acc) predicate t newElem
                | h :: t -> replace (h :: acc) predicate t newElem
                | [] -> List.rev acc 


        let rec private mapWhere acc predicate mapFunction collection =
            match collection with
                | h :: t when predicate(h) -> mapFunction(h) :: acc
                | h :: t -> h :: acc
                | []     -> acc


        let contains (elem: 'a)(lst : 'a list) = List.exists(fun x -> x = elem) lst

        
        let splitBy func = 
            let rec apply acc lst =
                match lst with
                | h :: t when func(h) -> [List.rev (acc); h :: t]
                | h :: t -> apply (h :: acc) t
                | []     -> [List.rev(acc)]

            apply [] 
                



        type List<'a> with
            static member replace (predicate: 'a -> bool) newElem collection = replace [] predicate collection newElem
            static member tryHead (list : 'a list) = if list.IsEmpty then None else Some list.Head
            static member count(pred)(list : 'a list) = (List.filter(pred) list) |> List.length
            static member mapWhere pred mapFunc list = mapWhere [] pred mapFunc list 
            static member fill(cnt : int)(value : 'a) = [1 .. cnt] |> List.map(fun x -> value)
            static member split func list = splitBy func list
            