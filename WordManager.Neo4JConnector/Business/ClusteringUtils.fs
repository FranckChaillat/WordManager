namespace WordManager.Grapher.Clustering

module ClusteringUtils =

    open Grapher.Entities.Items
    open WordManager.Framework.Tools.ListExtensions


    let getTransition(graph : Graph) : int[,] =
        let matrix = Array2D.zeroCreate graph.Length graph.Length
        
        let rec apply nodes = 
            match nodes with
            | h :: t -> 
                let nextNodes = h.outputNodes
                let collect = (fun i -> nextNodes |> List.count(fun (n, _) -> i = n.id))
                let occurrences = [0 .. graph.Length - 1] 
                                  |> Seq.map (fun i -> i, collect i)
            
                let appender = (Array2D.set matrix h.id)
                for (index, value) in occurrences do
                    appender index value
                apply t
            | [] -> matrix

        apply (Graph.nodes graph)

    
    let normalize (arr : int[,]) =
       let len = Array2D.length1(arr) 
       arr |> Array2D.map(fun y -> (float(y) / float(len)))
       
        