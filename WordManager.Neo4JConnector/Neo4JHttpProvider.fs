namespace Neo4JConnector
module Neo4JHttpProvider =
    
   open FSharp.Data
   open FSharp.Configuration
   open System
   
   
   type Settings = AppSettings<"app.config">
   
       
   let httpRequest content =

            let encoded = Text.Encoding.UTF8.GetBytes(Settings.Neo4jlogin+":"+Settings.Neo4jpw)
                           |> Convert.ToBase64String
            
            Http.RequestString("http://localhost:7474/db/data/transaction/commit",
                                headers = [ "Content-Type", "application/json";
                                            "Accept", "application/json; charset=UTF-8";
                                            "Authorization", "Basic "+encoded],
                                body = TextRequest content)
 
     