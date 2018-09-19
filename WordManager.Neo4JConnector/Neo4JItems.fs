namespace Neo4JConnector
module Items =

    open Framework.Helpers.Operators
    open Framework.Helpers.ListExtensions
    open System
         

    (******Type definition******)
    type Direction = 
        |LeftToRight
        |RightToLeft
        |None
    

    type Edge = { 
      name : string
      nextNode : Node
      direction : Direction 
    }
        

    and Node =  {
        id : int
        label : string option
        properties : (string * obj) list
        outputNodes : Edge list
    }
    with static member link = GraphHelper.createLinkFromNode(LeftToRight)
         static member linkLeft = GraphHelper.createLinkFromNode(RightToLeft)


    and Graph = private {
        nodelst : Node list
        outputNode : Node
    }  with static member link = GraphHelper.createLinkFromGraph(LeftToRight)

            static member linkLeft = GraphHelper.createLinkFromGraph(RightToLeft)
            
            static member nth (graph: Graph)(i) = 
                    if i > graph.nodelst.Length then
                        Option.None
                    else 
                        graph.nodelst.[i] |> Some
            static member Graph(nodelst, output) =
                {nodelst = nodelst; outputNode = output}

            static member nodes g = g.nodelst
            
            static member getOutputNode g = g.outputNode
                        
            member this.Head = this.nodelst.Head

            member this.Length = this.nodelst.Length

    

    and private GraphHelper =
        static member updateIds nodeLst1 (nodeLst2: Node list) =
            let updateNodeId newId node = {node with id = newId}
            let maxId = (List.maxBy(fun x -> x.id) nodeLst1).id
            let nodeLst2Updated = [maxId+1 .. (maxId + nodeLst2.Length)] 
                                         |> List.zip nodeLst2
                                         |> List.map(fun (node, i) ->
                                                    let edgeUpdated = node.outputNodes |> List.map(fun e -> { e with nextNode = updateNodeId (e.nextNode.id + maxId + 1) e.nextNode})
                                                    {node with id = i; outputNodes = edgeUpdated})
            nodeLst2Updated


        static member createLinkFromNode (direction)(baseNode, lnkName, output) =
            //update output with new id}
            let outputUpdated = {output with id = 1} 
            let edge = { name = lnkName; nextNode = outputUpdated; direction = direction }
            let baseNodeUpdated = {baseNode with outputNodes = edge :: baseNode.outputNodes; id = 0}

            Graph.Graph([baseNodeUpdated; outputUpdated],  outputUpdated)


        static member createLinkFromGraph (direction)(baseGraph, lnkName, output) =
            let newId = (baseGraph.nodelst |> List.maxBy(fun x -> x.id)).id |> (+) 1
            let newOutput = {output with id = newId}
            let edge = {name = lnkName; nextNode = newOutput; direction = direction}
            let updated = {baseGraph.outputNode with outputNodes = edge :: Graph.getOutputNode(baseGraph).outputNodes}
           
            let newNodes = List.replace (fun n -> n.id = updated.id) updated baseGraph.nodelst
            { nodelst =  newNodes @ [newOutput]; outputNode = newOutput }

     
     type Path = Node list


    (****Extractors****)

    let (|LeftArrow|_|) (edge: Edge) =
        if edge.direction = Direction.RightToLeft then
            (edge.name, edge.nextNode) |> Some
        else Option.None

    let (|RightArrow|_|) (edge: Edge) =
        if edge.direction = Direction.LeftToRight then
            (edge.name, edge.nextNode) |> Some
        else Option.None
    
    
    (******Functions******)
    let inline (==) (n1: Node)(n2: Node) = (n1.label = n2.label) && (n1.properties = n2.properties)

    let concatGraph (g1:Graph) (g2: Graph) =
        //concatening graphs
        let rec concat nodeLst1 nodeLst2 =
            match nodeLst2 with
                | h :: t ->  if not <| (List.exists (fun n -> n == h) <| nodeLst1) then
                                concat (h :: nodeLst1) t
                             else
                                let commonNode = nodeLst1 |> List.find (fun n -> n == h)
                                let updated = {h with outputNodes = commonNode.outputNodes @ h.outputNodes}
                                let res = nodeLst1 |> List.replace (fun x -> x == h) updated
                                concat res t
                | [] -> { g1 with nodelst = nodeLst1}
        
        let idsUpdated = GraphHelper.updateIds g1.nodelst g2.nodelst
        concat g1.nodelst idsUpdated
       
        
    let rec inline (.+) (g1: Graph)(g2: Graph) = concatGraph g1 g2
        

    let inline (---) (n: ^t when ^t : (static member link : (^t * string * Node -> Graph)))(edgeName) =  (n, edgeName)

    
    let inline (-->) (a : ^t, lnkName : string)(out: Node) = (^t : (static member link :  (^t * string * Node -> Graph))())(a, lnkName, out)

    
    let inline (<--) (out: Node)(a : ^t, lnkName : string) = (^t : (static member linkLeft :  (^t * string * Node -> Graph))())(a, lnkName, out)


    let EmptyNode  =   {id = 0; label = Option.None ; properties = []; outputNodes = [] }

                            
    let LabeledNode label = {id = 0; label = Some label ; properties = []; outputNodes = [] }     


    let PropertyNode properties = {id = 0; label = Option.None; properties = properties; outputNodes = []}


    let Node label (properties)  =
        {id = 0 ; label = Some label; properties = properties; outputNodes = []}


    let NodesToGraph (nodes: Node list) = if nodes.IsEmpty then 
                                              Option.None
                                          else 
                                             { nodelst = nodes; outputNode = nodes.Head } |> Some

    