namespace WordManager.Tagger.Crawler
module GenericCrawler =

    open FSharp.Data
        
    type Crawler(getTypeStrategy: HtmlDocument -> string option,
                 getGenderStrategy: string -> Map<string, string>,
                 getLinkedWordStrategy: HtmlDocument -> string seq,
                 word: string,
                 content : HtmlDocument) =
        member this.Word = word
        member this.GetContent = content
        member this.GetType = getTypeStrategy
        member this.GetLinkedWords = getLinkedWordStrategy
        member this.GetGender = getGenderStrategy
