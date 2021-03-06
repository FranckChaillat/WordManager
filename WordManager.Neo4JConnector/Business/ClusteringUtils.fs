﻿namespace WordManager.Grapher.Clustering

module ClusteringUtils =

    open Grapher.Entities.Items
    open Grapher.Entities.MclOptions
    open WordManager.Framework.Tools.ListExtensions
    open Grapher.Entities


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
       
    
    let expand(option : MclOptions)(matrix : float[,]) : float[,] =
        
        let expanded = Array2D.zeroCreate (Array2D.length1 matrix) (Array2D.length2 matrix)

        let apply (currentMat: float[,]) =
            for i in [0..(Array2D.length1 matrix-1) ] do
                for j in [0..(Array2D.length2 matrix-1) ] do
                    let row = matrix.[i, *]
                    let col = currentMat.[*, j]
                    Array.zip row (col)
                    |> Array.map(fun (a,b) -> a * b)
                    |> Array.sum
                    |> Array2D.set expanded i j
         
        let rec recursivlyExpand expandFactor currentMat =
            if expandFactor = 0 then
                apply currentMat
            else 
                apply(currentMat)
                recursivlyExpand (expandFactor-1) expanded
        
       
        let (expansionFactor, _) = MclOptions.get(option)
        recursivlyExpand (expansionFactor) matrix
        expanded


    let inflate(option : MclOptions)(matrix : float[,]) =
        let (_, inflationFactor) = MclOptions.get(option)
        let inflated = Array2D.map(fun x -> x ** float(inflationFactor)) matrix

        let sum = [| for e in [0.. Array2D.length2(inflated) - 1 ] do
                        yield Array.sum(inflated.[*, e]) |]

        let normalized = [| for e in [0 .. Array2D.length2(inflated) - 1] do
                            yield [| for i in inflated.[*, e] do 
                                       yield   i / (float(sum.[e])) |] |]
        
        let finalArray = Array2D.zeroCreate (Array2D.length1 matrix) (Array2D.length2 matrix)
        normalized |> Array.iteri(fun i e -> e |> Array.iteri(fun j v -> Array2D.set finalArray i j v))
        finalArray 