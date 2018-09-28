namespace WordManager.Grapher.Clustering

module ClusteringUtils =

    open Grapher.Entities.Items
    open WordManager.Framework.Tools.Seq


    let count(predicate : 't -> bool)(collection : 't seq) = 
        let rec f count (col: 't list) =
            match col with
            | h :: t when predicate h ->
                f (count + 1) t
            | _ :: t -> 
                f count t
            | _ -> count
        
        f 0 (collection |> List.ofSeq)


    let getTransition(graph : Graph) =
        let matrix = Array2D.init graph.Length graph.Length (fun x y -> 0)

        let rec apply nodes = 
            match nodes with
            | h :: t -> 
                let nextNodes = h.outputNodes
                let collect = (fun i -> nextNodes |> count(fun (n, _) -> i = n.id))
                let occurrences = [0 .. graph.Length - 1] 
                                  |> Seq.map (fun i -> i, collect i)
            
                let appender = (Array2D.set matrix h.id)
                for (index, value) in occurrences do
                    appender index value
                apply t
            | [] -> matrix

        apply (Graph.nodes graph)