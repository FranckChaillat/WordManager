namespace Grapher.Business
module PathFinding =

    open Grapher.Entities.Items
    open WordManager.Framework.Tools.ListExtensions
    open WordManager.Framework.Tools.Operators

     (****Functions for path discovery****)
    let rec nodeLookup path node =
        match path with
        | (h :: t) as part when h == node -> part
        | [] -> []
        | h :: t -> nodeLookup t node


    let splitPaths =
        let rec apply acc (paths : Path list) node = 
            match paths with
            | h :: t -> 
                match (nodeLookup h node) with
                    | [] -> apply acc t node
                    | subpath -> apply (subpath :: acc) t node
            | [] -> acc
        apply []


    (*Ajoute un noeud "avant" aux chemins existant; ajout le noeud à partir du noeud de branchement indiqué*)
    let branchHead = 
        let rec apply acc (paths: Path list)(nodeToMatch : Node)(newNode : Node)(relationName) = 
            match paths with
            | path :: tail ->
                let index = path |> List.tryFindIndex (fun x -> x == nodeToMatch)
                if index.IsNone then
                    apply acc tail nodeToMatch newNode relationName
                else
                    let sndPart = (Seq.skip index.Value path) |> List.ofSeq
                    apply ((newNode :: sndPart) :: acc) tail nodeToMatch newNode relationName
            | [] when acc.IsEmpty -> Option.None
            | _ -> Some acc

        apply []
                     

    (*Ajoute un noeud "aprés" aux chemins existant; ajoute le noeud à partir du noeud de branchement indiqué*)
    let branchTail (initPaths: Path list) (nodeToMatch: Node) (newNode : Node)
                   (relationName : string) (relationProperties : (string * obj) list) : Path list option = 
                   
        let rec apply acc (paths : Path list) nodeToMatch newNode relationName relationProperties =
            match paths with
            | ((_ :: _) as path) :: tail ->
                        match List.rev path with
                        | (h :: _) as rev when h == nodeToMatch -> 
                            let edge = {name = relationName; properties = relationProperties; direction = LeftToRight}
                            let updated = {rev.Head with outputNodes = (newNode, edge) :: rev.Head.outputNodes}
                            let appended = List.ofSeq (Seq.take (path.Length - 1) path) 
                                           @ [updated; newNode]
                            apply (appended :: acc) tail nodeToMatch newNode relationName relationProperties
                        | _ -> 
                            apply (acc) tail nodeToMatch newNode relationName relationProperties
            | _ when acc.IsEmpty -> Option.None
            | _ -> Some acc

        apply [] initPaths nodeToMatch newNode relationName relationProperties
    


    let rec getGraphFullPaths (acc: Path list) (nodeLst : Node list) : Path list =
        match nodeLst with
        | h :: t -> let nxtNodes = h.outputNodes
                    let paths = nxtNodes 
                                 |> List.fold(fun a (n, e) ->
                                        let newPaths = (branchTail acc h n e.name e.properties) |? []
                                                        @ ((branchHead acc n h e.name) |? [])

                                        if not newPaths.IsEmpty then
                                            a |> Seq.skipWhile (fun x -> List.rev(x).Head == h)
                                              |> List.ofSeq
                                              |> (@) (newPaths |> List.filter (fun e -> (contains e  a) |> not))
                                        else 
                                            a @ [[h; n]]
                                    ) acc
                    getGraphFullPaths (paths) t
        | []     -> acc


    (*Evalue si path2 est contenu dans path1*)
    let isSubPath(path1) (path2) =
        path2 |> List.forall(fun n -> path1 |> List.exists(fun x -> n == x))


    (*Evalue si lz noeud donné est contenu dans le chemin donné*)
    let pathContains path node =
        (path |> List.exists(fun x -> x == node))


    (*Retourne les morceaux du chemin présent dans path2 et non présent dans path1; fait la différence des deux chemins*)
    let minus(path1: Path) =                            
        let path1Contains = pathContains path1
        let isSubPathOfPath1 = isSubPath path1
        let updatePathList (acc: Path list) (currentPath: Path) = 
            if acc.IsEmpty then [[currentPath.Head]] else (acc.Head @ [currentPath.Head]) :: acc.Tail

        let rec apply (acc: Path list) (previousNode : Node Option) (path2 : Path) =
             match path2 with
             | [] -> acc
             | (h :: tail) as all when isSubPathOfPath1 all ->  updatePathList acc path2

             | h :: tail when not (path1Contains h) ->
                     let accUpdated = updatePathList acc path2
                     apply accUpdated (Some h) tail

             | h :: tail when previousNode.IsSome ->
                    let accUpdated =  acc |> (fun x -> if not (path1Contains previousNode.Value) then
                                                          updatePathList x path2
                                                       else x )
                                          |> (fun x -> if not tail.IsEmpty && not (path1Contains tail.Head) then
                                                          x.IsEmpty ?= ([[h]], [h] :: x)
                                                       else x )
                    apply accUpdated (Some h) tail

             | h :: tail ->  apply [] (Some h) tail
                    
        apply [] Option.None



    let pathEqual (path1: Path) (path2: Path) =
        if path1.Length <> path2.Length then
             false
        else
             (List.zip path1 path2)
             |> List.forall(fun (a,b) -> a == b)
            



    let getGraphPaths (graph) =
        let rec getDistinct (acc : Path list)(firstItem : Path)(otherItems: Path list) =
            match otherItems with
            | h :: tail -> let minusRes = minus firstItem h
                           let newAcc = acc @ (minusRes |> List.filter(fun x -> not (acc |> List.exists(fun n -> pathEqual n x))))
                           getDistinct newAcc firstItem tail
            | []        -> acc

        match (getGraphFullPaths [] (Graph.nodes graph)) with
        | h :: tail -> getDistinct [h] h tail
        | [] -> []
                    

       



       

