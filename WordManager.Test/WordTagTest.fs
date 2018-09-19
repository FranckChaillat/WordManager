namespace WordTaggerTest
module WordTrackingTest=

    open Xunit
    open WordTagger.WordTracking
    open Model.FrenchModel
    open System.Net
    open System.Text
    open FSharp.Data
    open WordTagger
    

    let getWebContent word = 
        let url = "http://fr.wiktionary.org/wiki/"+word
        let wc = new WebClient()
        let data = wc.DownloadData(url)
        let download = Encoding.UTF8.GetString(data)
        download

    
    let getHtmlDocument word = 
        HtmlDocument.Load("https://fr.wiktionary.org/wiki/"+word)   


    [<Fact>]
    let ShouldBe_a_Noun() =
        let crawler = WikiCrawler.getCrawler("chat")
        let res = getHtmlDocument "chat" |> crawler.GetType
        Assert.True(res.IsSome)
        let isNoun = res.Value = "Nom_commun_1"
        Assert.True(isNoun)
    
    [<Fact>]
    let ShouldBe_a_Noun2() =
        let crawler = WikiCrawler.getCrawler("language")
        let res = getHtmlDocument "language" |> crawler.GetType
        Assert.True(res.IsSome)
        let isNoun = res.Value = "Nom_commun"
        Assert.True(isNoun)
    
    [<Fact>]
    let ShouldBe_an_Adjective() =
        let crawler = WikiCrawler.getCrawler("brillant")
        let res = getHtmlDocument "brillant" |> crawler.GetType
        Assert.True(res.IsSome)
        let isAdjective = res.Value = "Adjectif"
        Assert.True(isAdjective)

    [<Fact>]
    let ShouldBe_an_Adjective2() =
        let crawler = WikiCrawler.getCrawler("grand")
        let res = getHtmlDocument "grand" |> crawler.GetType
        Assert.True(res.IsSome)
        let isAdjective = res.Value = "Adjectif"
        Assert.True(isAdjective)
    
    [<Fact>]
    let ShouldBe_an_Adjective3() =
        let crawler = WikiCrawler.getCrawler("RaDiN")
        let res = getHtmlDocument "grand" |> crawler.GetType
        Assert.True(res.IsSome)
        let isAdjective = res.Value = "Adjectif"
        Assert.True(isAdjective)

    [<Fact>]
    let ShouldBe_first_group_verb() =
        let crawler = WikiCrawler.getCrawler("grandir")
        let res = getWordDescripton crawler
        let isOk = res.IsSome && res.Value |> function |{id = _; text = "grandir"; word = Verb("grandir", (Some(Infinitive), Some(Second)))::_ ; linkedTo = h :: t } -> true
        Assert.True(isOk)
    
    [<Fact>]
    let ShoultBe_third_group_verb() = 
        let crawler = WikiCrawler.getCrawler("prendre")
        let res = getWordDescripton crawler
        let isOk = res.IsSome && res.Value |> function {id = _; text = "prendre"; word = Verb("prendre", (Some(Infinitive), Some(Third))) :: _; linkedTo = h :: t} -> true
        Assert.True(isOk)


    [<Fact>]
    let Should_get_linkedWords() =
        let crawler = WikiCrawler.getCrawler("maison")
        let res =  crawler.GetLinkedWords(crawler.GetContent) |> Seq.toList
        let isOk = (res.Length > 0) && (res |> List.exists(fun x -> x.Length = 0) |> not)
        Assert.True(isOk)
 

    [<Fact>]
    let Should_get_linkedWords_for_verbs() =
        let crawler = WikiCrawler.getCrawler("acheter")
        let res =  crawler.GetLinkedWords(crawler.GetContent) |> Seq.toList
        let isOk = (res.Length > 0) && (res |>  List.exists(fun x -> x.Length = 0) |> not)
        Assert.True(isOk)



   
       