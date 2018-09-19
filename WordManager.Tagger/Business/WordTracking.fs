namespace WordTagger

module WordTracking=

    open Model.FrenchModel      
    open WordManager.Framework.Tools
    open Grapher.Entities.Items
    open WordManager.Common.DataProvider.Neo4JProvider
    open GenericCrawler

    type RawInfo = { word : string; content : string; textType : string option}

    (**Matchers of all kind of words**)
    let (|Adjective|_|) (content : RawInfo) =
        match content.textType with
            | Some "Adjectif" -> Some(content.word)
            | _ -> Option.None


    let (|Verb|_|) (content : RawInfo) =
        match content.textType with
            | Some "Verbe" when (content.word.EndsWith "ir") -> 
                Some(content.word, Infinitive, Second)
            | Some "Verbe" when (content.word.EndsWith "er") ->
                Some(content.word, Infinitive, First)
            | _ -> 
                Some(content.word, Infinitive, Third)


    let (|NC|_|)(crawler : Crawler)(content : RawInfo) =
        match content.textType with
            | Some "Nom_commun" ->
                content.content |> crawler.GetGender
                                |> Map.tryFind("gender")
                                |> (function |Some("masculin") -> (content.word, Sing, Male)
                                             | _ -> (content.word, Sing, Female))
                                |> Some
            | _ -> Option.None


    (**Gets the tag correponding to the word by analysing the content of the wiki page**)
    let getWordDescripton (crawler : Crawler): WordTag option =
        let textType = crawler.GetContent |> crawler.GetType
        let htmlString, word = crawler.GetContent.ToString(), crawler.Word

        let wordType = match { word = word; content = htmlString; textType = textType } with
                        | Verb(name, setting, group) -> 
                            Verb(name, (Some(setting), Some(group))) |> Some
                        | NC crawler (name, time, gender) ->
                            Noun(name, (Some(time), Some(gender))) |> Some
                        | Adjective(name) ->
                            Adjective(name) |> Some
                        | _ -> Option.None
        wordType |> Option.map(fun x -> { id = 0 
                                          text = word
                                          word = [x]
                                          linkedTo = (crawler.GetLinkedWords crawler.GetContent) |> Seq.toList } )


    (**Append word to word database**)
    //let appendWord (word: WordTag) =
    //   word.word |> List.map(Word.format)
    //             |> List.map(fun x -> let optionGraph = nodesToGraph [(node "Word" x)]
    //                                  optionGraph |> Option.map(fun g -> merge g))
    //             |> ignore
      
      
    //let getWord (word : string) =
    //     nodeToGraph (labeledNode "Word")
    //     |> ``merge``
                        

    