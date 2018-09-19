namespace WordTagger

module WordTaggerService =

    open Suave
    open Suave.Successful
    open FSharp.Data
    open SettingsEcosystem


    //let startWordTag (depth)(word)  =
    //    let baseUri = Settings.Wordressource.AbsolutePath

    //    let rec collect (currentLevel: int) (currentWord: string) : string =
    //        let crawler = WikiCrawler.getCrawler(currentWord)
    //        let descr = getWordDescripton crawler
    //        descr |> Option.map (fun tag -> appendWord tag) |> ignore
    //        match descr |> Option.map (fun x -> x.linkedTo) with
    //        | Some(h :: _) when currentLevel < depth -> collect (currentLevel + 1) h
    //        | _ -> currentWord
        
    //    OK ""

    //(**Add a word to word database**)
    //let postWordTag(word : string) = 
    //    let res = WikiCrawler.getCrawler(word)
    //              |> getWordDescripton
    //              |> Option.map(fun x -> appendWord x)
    //    if res.IsNone then
    //      RequestErrors.UNPROCESSABLE_ENTITY("The given word not exist or is not supported by the crawler.")
    //    else 
    //      OK("Word added to the word database.")

    
    
    //(**Get an existing word tag**)
    //let getWordTag (word: string) =
    //    OK("")

