namespace WordManager.Tagger.Parser
module Wildcard =

    open WordManager.Tagger.Model.FrenchModel
    open FSharp.Text.RegexProvider

    type private SyntaxRegex = Regex<"{(?<id>\d+:(?<class>NOUN|ADJ|PRON|VERB)\[(?<properties>.*)](\((?<function>SUBJECT|COMPLEMENT)?\))?->(?<target>\d+))};?">

    type WildCardItem = private {
        id : int
        wordClass : WordModel
        syntaxicFunc : SyntacticFunction
    }

    type WildCard = {
        items : WildCardItem seq
    }

    let private matchWildCardItem (element: string)=
        let mtch = SyntaxRegex().TypedMatch(element.Trim())
        let ``class`` = mtch.``class``.Value.ToLowerInvariant()
        let properties = mtch.properties.Value.ToLowerInvariant().Split(',')
        let id = mtch.id.Value
        let ``function`` = if mtch.``function``.Success then Some(mtch.``function``.Value) else None
        id, ``class``, ``function``, properties

    //let private tryParseNoun pattern =
    //    match matchWildCardItem(pattern) with
    //    | id, ("NOUN" | "PRONOUN") , f, props -> 
    //       let time = props 
    //                  |> Array.filter(Time.isValid)
    //                  |> Array.tryHead
    //                  |> Option.bind(Time.getFromText)

    //       let gender = props 
    //                    |> Array.filter(Gender.isValid)
    //                    |> Array.tryHead
    //                    |> Option.bind(Gender.getFromText)

            
    //       let wordClass : WildCardItem = { 
    //           id = id
    //           wordClass = WordModel.Noun(time, gender)
    //           syntaxicFunc = { FunctionType. }
               
    //       }
           
    //       Result.Ok("")
 

    //let parse (pattern : string) : WildCard option =
    //    let mtch = SyntaxRegex().TypedMatch(pattern.Trim())
    //    let parts = mtch.properties.Value.Split(';')
    //    parts |> Seq.map (fun p -> 
    //        match p with
    //        |Noun())
        