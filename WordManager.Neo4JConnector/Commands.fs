namespace Neo4JConnector
module Commands =

    open Neo4j.Driver.V1
    open FSharp.Configuration
    open System
    open Utils
    open Neo4JConnector.Neo4JHttpProvider
    open System.Text.RegularExpressions

    type Settings = AppSettings<"app.config">
                   
                                                 
    let private driverObject = lazy(GraphDatabase.Driver(Settings.ConnectionStrings.Neo4jWordBank, AuthTokens.Basic(Settings.Neo4jlogin, Settings.Neo4jpw)))
    

    let readResults (statementResult : IStatementResult) =
        [for res in statementResult do
            let keys = [for k in res.Keys do yield k, res.[k]]
            yield keys]
    


    (*Execute une requête unique*)
    let exec (query : string) =
        query
         |> driverObject.Value.Session().Run
         |> readResults


    (* Formate les requêtes données et retourne leur représentation en Json *)     
    let asJson (statements : string list) = 
        let statements = List.fold(fun acc x -> 
                                        "{ \"statement\" : \""+x+"\"}" :: acc
                                  ) [] statements

        let groupedStatements = String.Join(",", List.rev statements)
        let fullJson = "{ \"statements\" :  ["+groupedStatements+"]}"
        fullJson             



    (*Construction de la représentation textuelle du graphe et ajout de la clause return pour chacun des chemins*)
    let private Format addReturn operation graph =
        let pathStr = graphToString graph
        //extraction des noms des noeuds 
        if addReturn then
            pathStr |> List.map(fun path -> Regex.Matches(path, @"\((?'nodename'n\d+)"))
                    |> List.map(fun mtchCol ->  let nodeLabels = {0 .. mtchCol.Count - 1} 
                                                                |> Seq.map(fun i -> let value = (mtchCol.Item i).Groups.["nodename"]
                                                                                    if value.Success then value.Value else "")
                                                String.Join(" , ", nodeLabels))
                    |> List.zip pathStr
                    |> List.map(fun (mergeStatement, returnSatement) ->  String.Format(operation + " {0} RETURN {1}", mergeStatement, returnSatement))
        //sinon préfixage de l'opération
        else 
            pathStr |> List.map(fun p -> operation + " " + p)


    let formatMerge = Format false "MERGE"

    let formatMatch = Format true "MATCH"

    let merge = formatMerge >> asJson >> httpRequest

    let ``match`` = formatMatch >> asJson >> httpRequest



