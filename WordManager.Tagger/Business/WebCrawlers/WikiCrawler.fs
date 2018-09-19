namespace WordManager.Tagger.Crawler

module WikiCrawler =
        
    open System.Text.RegularExpressions
    open FSharp.Data    
    open WordManager.Framework.Tools
    open GenericCrawler
    open System

    (**Functions used to match html content **)
    let private regexMatch pattern (matcher: (string -> string -> (string * Group) seq)) content =
        (matcher pattern content) 
                |> Seq.filter(fun (_, group) -> group.Success)
                |> Seq.groupBy(fun (k, _) -> k )
                |> Seq.map(fun (k, seqOfValues) -> k, System.String.Join(",", seqOfValues |> Seq.map(function (a,b) -> b.Value)))
                |> Map.ofSeq

            
    let private getWordType (document : HtmlDocument) =
        document.Descendants ["h3"] 
            |> Seq.tryItem 1
            |> Option.map (fun x -> x.Descendants ["span"])
            |> Option.bind (fun x -> Seq.tryHead x)
            |> Option.bind (fun x -> x.TryGetAttribute("id"))
            |> Option.map (fun x -> x.Value())
    

    let private getLinkedWords (htmlDoc : HtmlDocument) =
        htmlDoc.Descendants ["ol"]
        |> Seq.collect(fun x -> x.Descendants ["li"])
        |> Seq.map(fun x -> let mtch =  Regex.Match(x.ToString(), "<li>(?<test>.*)<ul>", RegexOptions.Singleline).Value
                            if mtch.Length > 0 then
                                let parsed = HtmlNode.Parse(mtch).Head
                                let linkText = parsed.Descendants ["a"] |> Seq.map(fun e -> e.InnerText())
                                String.concat " " linkText + parsed.DirectInnerText()
                            else "" )
        |> Seq.where (fun row -> row.Length > 0)
        |> Seq.collect (fun row -> row.Split(' '))
        |> Seq.where (fun word -> word.Length > 0)



    let private getWordGender = regexMatch "<p><b>.*<\/b>.*<span class=\"ligne-de-forme\"><i>(?<gender>féminin|masculin)"
                                 (fun pattern input -> Regex.Match(input, pattern)
                                                     |> fun x -> if x.Success then seq [("gender", x.Groups.["gender"])]
                                                                 else seq[])

        
    let getCrawler word = 
        let lowered = word |> Seq.map(fun e -> Char.ToLowerInvariant(e)) |> String.Concat
        let document = HtmlDocument.Load("http://fr.wiktionary.org/wiki/"+lowered)
        Crawler(getWordType, getWordGender, getLinkedWords, word, document)