namespace WordManager.Common.DataProvider
module Neo4JProvider =

    open Neo4j.Driver.V1
    open System
    open System.Text.RegularExpressions
    open Grapher.Business.Utils
   

    (* Format given requests et turns it into json *)     
    let asJson (statements : string list) = 
        let statements = List.fold(fun acc x -> 
                                        "{ \"statement\" : \""+x+"\"}, \"resultDataContents\" : [\"graph\"] " 
                                        :: acc
                                  ) [] statements

        let groupedStatements = String.Join(",", List.rev statements)
        let fullJson = "{ \"statements\" :  ["+groupedStatements+"]}"
        fullJson


    let private matchPathItem (groupname: string) regex pathtext =
        pathtext |> List.map(fun path -> Regex.Matches(path, regex))
                 |> List.map(fun mtchCol -> 
                        let lables = {0 .. mtchCol.Count - 1} 
                                     |> Seq.map(fun i -> let nodeValue = (mtchCol.Item i).Groups.[groupname]
                                                         if nodeValue.Success then nodeValue.Value else "")
                        String.Join(" , ", lables))


    (* Graph text representation and return clause addition for each path *)
    let private format addReturn operation graph =
        let pathStr = graphToString graph
        // nodes and edges name extraction 
        if addReturn then
            let nodes = (matchPathItem "nodename" "\((?'nodename'n\d+)" pathStr)
            let edges = (matchPathItem "edgename" "\[(?'edgename'e\d+):" pathStr)
            List.zip3 nodes edges pathStr
              |> List.map(fun (n, e, path) ->  path, (String.concat " , " [n;e])) 
              |> List.map(fun (mergeStatement, returnSatement) ->  String.Format(operation + "{0} RETURN {1}", mergeStatement, returnSatement))
        else 
            pathStr |> List.map(fun p -> operation + " " + p)

    
    let formatMerge = format false "MERGE"


    let formatMatch = format true "MATCH"


    let merge = formatMerge >> asJson >> Neo4JHttpProvider.commit


    let ``match`` = formatMatch >> asJson >> Neo4JHttpProvider.commit
    
    
    //let neo4jProvider = 
    //    GenericProvider.GenericProvider((formatMerge >> asJson), (formatMerge >> asJson), (formatMerge >> asJson))