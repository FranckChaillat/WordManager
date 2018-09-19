namespace WordManager.Common.DataProvider

module Neo4JHttpProvider =
    
   open FSharp.Data
   open System
   open WordManager.Common.Configuration

   //httpRequest : string -> string -> string
   let private httpRequest method requestPath content =
            let encoded = Text.Encoding.UTF8.GetBytes(DataBaseConf.Neo4JSettings.Neo4jlogin+":"+DataBaseConf.Neo4JSettings.Neo4jpw)
                           |> Convert.ToBase64String

            Http.RequestString(requestPath,
                                headers = [ "Content-Type", "application/json";
                                            "Accept", "application/json; charset=UTF-8";
                                            "Authorization", "Basic "+encoded],
                                httpMethod = method,             
                                body = TextRequest content)
 
   
   let commit =
       String.Format("http://{0}:{1}{2}", DataBaseConf.Neo4JSettings.Host,
                                          DataBaseConf.Neo4JSettings.Port, 
                                          DataBaseConf.Neo4JSettings.CommitPath)
       |> httpRequest "POST"

