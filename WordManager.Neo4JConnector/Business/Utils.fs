namespace Grapher.Business
module Utils = 
  
    open Grapher.Entities.Items
    open Grapher.Business.PathFinding
    open System
    open WordManager.Framework.Tools.Operators
    open WordManager.Framework.Tools.ListExtensions
    open FSharp.Data
    open Grapher.Entities


    let parseJsonProperties propertyString = 
        let jsObject = JsonValue.Parse(propertyString)
        jsObject.Properties() |> Array.map(fun (a,b) -> a, b:> obj)
                              |> List.ofArray
          

    (**Utils to parse json response objects to Graph**)
    let jsonToGraph jsonResponse =
        let res = Neo4JJson.Parse(jsonResponse).Results
        let (nodes, rels) = Array.tryHead res.[0].Data
                               |> Option.map(fun data -> data.Graph.Nodes, data.Graph.Relationships)
                               |? ([||], [||])

        // parse nodes from json response
        let parsedNodes = nodes 
                          |> Array.map(fun n -> { id = n.Id
                                                  label = Array.tryHead(n.Labels)
                                                  outputNodes = []
                                                  properties = []
                                                })
        let nodesMap = parsedNodes |> Array.map(fun n -> n.id, n) |> Map.ofArray

        //builds graph              
        let rec buildGraph (nodeList : Node list)(graph: Graph option) =
            match nodeList with
            | currentNode :: tail -> 
                let outputNodes = rels |> Seq.filter(fun (rel) -> rel.StartNode = currentNode.id)
                                       |> Seq.map(fun rel -> 
                                            Map.tryFind rel.EndNode nodesMap
                                            |> Option.map(fun outnode -> outnode, { name = rel.Type
                                                                                    properties = parseJsonProperties(rel.Properties.JsonValue.ToString())
                                                                                    direction = LeftToRight
                                                                                   }))
                                       |> Seq.filter(fun x -> x.IsSome)
                                       |> Seq.map(fun x -> x.Value)
                if Seq.length(outputNodes) > 0 then
                    let node, edge = Seq.head(outputNodes)
                    let childNodes = Seq.tail(outputNodes)
                                     |> Seq.fold(fun g (n, e) ->  g .+ (currentNode --- e.name --> n))
                                                (currentNode --- edge.name --> node)
                    graph |> Option.map(fun g -> g .+ childNodes)
                          |? childNodes
                          |> fun g -> buildGraph tail (Some g)
                else buildGraph tail graph

            | [] -> graph
        buildGraph (parsedNodes |> Array.toList) Option.None
            

    let formatProperties (props : (string * obj) list) =
        if props.Length > 0  then
            let formated =  props 
                             |> List.map(fun (a,b) -> a+ " : " + (if b :? string then "\'"+string(b)+"\'" else string b))
            "{"+String.Join(",", formated)+"}"

        else ""

    (**Utils to converts graph items to string**)                                           
    let edgeToString (label: string)(edge: Edge) : string = 
        "-["+label+":"+edge.name + formatProperties(edge.properties)+"]-"
        |> (match edge with
            | LeftArrow(_,_) -> (fun x -> "<" + x)
            | RightArrow(_,_) ->  (fun x -> x + ">"))

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
                                                                       |> List.tryFind(fun (n,e) -> n == next)
                                                                       |> Option.map(fun (n,e) -> edgeToString ("e"+ string(h.id) + string(edgeIndex)) e)
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
     
      

                                              
                    
        
        


        

