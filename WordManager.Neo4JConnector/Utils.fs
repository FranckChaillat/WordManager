namespace Neo4JConnector
module Utils = 
  
    open Neo4JConnector.Items
    open Neo4JConnector.PathFinding
    open System
    open Framework.Helpers.Operators
    open Framework.Helpers.ListExtensions

                                               
    let edgeToString (label: string)(edge) : string =
        match edge with
        | LeftArrow(name, next) -> "<-["+label+":"+name+"]-"
        | RightArrow(name, next) -> "-["+label+":"+name+"]->"
    

    
    let formatProperties (props : (string * obj) list) =
        if props.Length > 0  then
            let formated =  props 
                             |> List.map(fun (a,b) -> a+ " : " + (if b :? string then "\'"+string(b)+"\'" else string b))
            "{"+String.Join(",", formated)+"}"

        else ""


    (*Transforme un ou plusieurs noeud en une repésentation textuelle*)
    let nodesToString (nodes: Node list) =
        nodes |> List.map (fun n -> 
                                let label = "(n"+ string(n.id)+" : "+ (n.label |? " ")
                                let properties = formatProperties n.properties
                                label + properties + ")")    
    
    let nodeToString node =
        let label = "(n"+ string(node.id)+" : "+ (node.label |? " ")
        let properties = formatProperties node.properties
        label + properties + ")"

     
    let private getNextNodes (node : Node) = node.outputNodes |> List.map(fun x -> x.nextNode)

 
    (*Transforme un graphe en une représentation textuelle aprés extraction des différents chemins*)
    let graphToString graph =
        let rec pathToString namedNodes acc edgeIndex path = 
            match path with
            | h :: tail -> let nodeStr = namedNodes 
                                          |> List.tryFind(fun (n, str) -> n == h) 
                                          |> Option.map(fun (_,name) -> "("+name+")")
                                          |? nodeToString h 

                           let lnkTonext = List.tryHead(tail)
                                            |> Option.map(fun next -> h.outputNodes 
                                                                       |> List.tryFind(fun output -> output.nextNode == next)
                                                                       |> Option.map(fun e -> edgeToString ("e"+ string(h.id) + string(edgeIndex)) e)
                                                                       |? "")
                                            |? ""
                           pathToString (namedNodes) (acc + nodeStr + lnkTonext) (edgeIndex + 1) (tail)
            | []     -> acc


        let paths = getGraphPaths graph
        let flattenNodes = List.concat(paths)
        let redundant, uniques = flattenNodes 
                                   |> List.ofSeq 
                                   |> Seq.distinctBy(fun x -> (x.id, x.label, x.properties)) |> List.ofSeq
                                   |> List.partition (fun x -> (flattenNodes |> List.count(fun n -> x == n)) > 1)

        let named = redundant |> List.map(fun x -> x, "n"+ string(x.id))
        let prefixNodeDeclaration = nodesToString redundant
        let other = paths |> List.map (fun path -> pathToString named "" 0 path)
        prefixNodeDeclaration @ other



    (****Graph extention for path discovery****)
    type Graph with
        static member getFullPaths (graph: Graph) = getGraphFullPaths [] (Graph.nodes graph)
        static member getPaths (graph: Graph) = getGraphPaths graph
     
      

                                              
                    
        
        


        

