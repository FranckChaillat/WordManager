namespace Grapher.Entities
module Items =

    open WordManager.Framework.Tools.ListExtensions
    open WordManager.Framework.Tools.Operators

    (******Type definition******)
    type Direction = 
        |LeftToRight
        |RightToLeft
        |None
    

    type Edge = { 
      name : string
      properties : (string * obj) list
      direction : Direction
    }

    type Node =  {
        id : int
        label : string option
        properties : (string * obj) list
        outputNodes : (Node * Edge) list
    }
    with static member link = GraphHelper.createLinkFromNode LeftToRight []
        
         static member propertyLink = GraphHelper.createLinkFromNode LeftToRight
        
         static member linkLeft = GraphHelper.createLinkFromNode RightToLeft []

         static member propertyLinkLeft = GraphHelper.createLinkFromNode RightToLeft


    and Graph = private {
        nodelst : Node list
        outputNode : Node
    }  with static member link = GraphHelper.createLinkFromGraph LeftToRight []
            
            static member propertyLink = GraphHelper.createLinkFromGraph LeftToRight

            static member linkLeft = GraphHelper.createLinkFromGraph RightToLeft []

            static member propertyLinkLeft = GraphHelper.createLinkFromGraph RightToLeft
            
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
            let maxId = (Seq.maxBy(fun x -> x.id) nodeLst1).id
            let nodeLst2Updated = [maxId+1 .. (maxId + nodeLst2.Length)] 
                                    |> List.zip nodeLst2
                                    |> List.map(fun (node, i) ->
                                           let updated = node.outputNodes |> List.map(fun (n, e) ->  { n with id =  n.id + i}, e)
                                           { node with id = i; outputNodes = updated })
            nodeLst2Updated

        static member createLinkFromNode direction properties (linkLabel, baseNode, output) =
            let outputUpdated = {output with id = 1} 
            let edge = { name = linkLabel
                         properties = properties
                         direction = direction
                       }
            let baseNodeUpdated = {baseNode with id = 0
                                                 outputNodes = (outputUpdated, edge) :: baseNode.outputNodes}
            Graph.Graph([baseNodeUpdated; outputUpdated],  outputUpdated)

        static member createLinkFromGraph direction properties (linkLabel, baseGraph, output) =
            let newId = (baseGraph.nodelst |> List.maxBy(fun x -> x.id)).id |> (+) 1
            let newOutput = {output with id = newId}
            let edge = {name = linkLabel; properties = properties; direction = direction}
            let updated = {baseGraph.outputNode with outputNodes = (newOutput, edge) :: Graph.getOutputNode(baseGraph).outputNodes}
           
            let newNodes = List.replace (fun n -> n.id = updated.id) updated baseGraph.nodelst
            { nodelst =  newNodes @ [newOutput]; outputNode = newOutput }

     
     type Path = Node list


    (****Extractors****)

    let (|LeftArrow|_|) (edge: Edge) =
        if edge.direction = Direction.RightToLeft then
            (edge.name, edge.properties) |> Some
        else Option.None

    let (|RightArrow|_|) (edge: Edge) =
        if edge.direction = Direction.LeftToRight then
            (edge.name, edge.properties) |> Some
        else Option.None
    
    (******Private functions******)

    let private updateNodeIdentifier (n : Node) =
        { n with id = n.id - 1 ; 
                 outputNodes = n.outputNodes |> Seq.map(fun (n,e) -> { n with id = n.id - 1}, e) |> Seq.toList }

    (******Functions******)
    let inline (==) (n1: Node)(n2: Node) = (n1.label = n2.label) && (n1.properties = n2.properties)
    

    let concatGraph (g1:Graph) (g2: Graph) =
        //concatening graphs
        let rec concat nodeLst1 nodeLst2 =
            match nodeLst2 with
                | h :: tail ->  if not <| (List.exists (fun n -> n == h) <| nodeLst1) then
                                    concat (h :: nodeLst1) tail
                                else
                                    let commonNode = nodeLst1 |> List.find (fun n -> n == h)
                                    let updatedids = 
                                        h.outputNodes |> Seq.map(fun (n,e) -> { n with id = n.id - 1}, e) |> Seq.toList

                                    let updated = {h with id = commonNode.id; outputNodes = commonNode.outputNodes @ updatedids}
                                    let res = nodeLst1 |> List.replace (fun x -> x == h) updated

                                    concat res (tail |> List.map (updateNodeIdentifier))
                | [] -> { g1 with nodelst = nodeLst1}
        
        let idsUpdated = GraphHelper.updateIds g1.nodelst g2.nodelst
        concat g1.nodelst (idsUpdated |> Seq.toList)
       
        
    let rec inline (.+) (g1: Graph)(g2: Graph) = concatGraph g1 g2
        

    let inline (---) (n: ^t when ^t : (static member link : (string * ^t * Node -> Graph)))(edgeName) =  
        (edgeName, n)

    let inline (-&-) (n: ^t when ^t : (static member propertyLink : ((string * obj) list * string * ^t * Node -> Graph)))(edgeName, properties) =  
        (properties, edgeName, n)

    
    let inline (-&>) (a : ^t, lnkName : string, properties : (string * obj) list)(out: Node) = 
        (^t : (static member link :  ((string * obj) list * string * ^t * Node -> Graph))())(properties, lnkName, a, out)


    let inline (-->) (lnkName : string, a: ^t)(out: Node) = 
        (^t : (static member link :  (string * ^t * Node -> Graph))())(lnkName, a, out)
    
    let inline (<--) (out: Node)(lnkName : string, a : ^t) = 
        (^t : (static member linkLeft :  (string * ^t * Node -> Graph))())(lnkName,a , out)

    
    let inline (<&-) (out: Node)(a : ^t, lnkName : string, properties : (string * obj) list) = 
        (^t : (static member propertyLinkLeft :  ((string * obj) list * string * ^t * Node -> Graph))())(properties, lnkName, a, out)

    let emptyNode = {id = 0; label = Option.None ; properties = []; outputNodes = [] }

                            
    let labeledNode label = {id = 0; label = Some label ; properties = []; outputNodes = [] }     


    let propertyNode properties = {id = 0; label = Option.None; properties = properties; outputNodes = []}


    let node label (properties)  =
        { 
            id = 0 
            label = Some label
            properties = properties
            outputNodes = []
        }


    let nodesToGraph (nodes: Node list) =
        nodes.IsEmpty ?= (Option.None, { nodelst = nodes; outputNode = nodes.Head } |> Some)
           

    let nodeToGraph (node : Node) = 
        { 
            nodelst = [node]
            outputNode = node
        }

    
    