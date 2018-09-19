namespace WordManager.Tagger.Parser
module Parser =

    open WordManager.Tagger.Model.FrenchModel
    open System.IO
    open WordTagger.SettingsEcosystem
    open FSharp.Text.RegexProvider

    type SyntaxRegex =  Regex< @"{(?<element>\d+:(?<type>NOUN|ADJ|PRON|VERB)\[(?<properties>.*)]->(?<target>\d+))};?">

    (**Private functions**)
    let private patterns = lazy(  
        if not (File.Exists(Settings.SyntacticPatternFile)) then
            raise (IOException("The required pattern file doesn't exist at the given path"))
        else
            File.ReadAllText(Settings.SyntacticPatternFile).Split(';'))

    let private checkProperties wordType (properties : string seq) =
        let condition = match wordType with
                        | "adj" | "adjective" | "noun" | "commonnoun" ->
                            (fun p -> Time.isValid p || Gender.isValid p) |> Some
                        | "verb" -> 
                            (fun p -> VerbGroup.isValid p || VerbSetting.isValid p) |> Some
                        | _ -> None
        condition 
        |> Option.map(fun ``function`` -> properties |> Seq.forall(fun e -> ``function``(e)))
        |> Option.isSome

    let private parseElem element =
        let mtch = SyntaxRegex().TypedMatch(element)
        let properties = mtch.properties.Value.Split(';')
        let ``type`` = mtch.``type``.Value.ToLowerInvariant()
        let target = mtch.Success |> (function | true -> mtch.Value |> int |> Some |_ -> None)
        properties, ``type``, target

    (**Actives patterns**)
    let (|Adjective|_|) (element : string) =
        let (props, ``type``, target) = parseElem element
        match (checkProperties ``type`` props), ``type``, target with
        | true, ("adj" | "adjective"), Some(targetId) ->
            
            Adjective(props |> Seq.tryPick(Time.getFromText),
                      props |> Seq.tryPick(Gender.getFromText),
                      targetId)
            |> Some
        | _ -> None


    let (|Verb|_|) (element : string) =
        let (props, ``type``, target) = parseElem element
        match (checkProperties ``type`` props), ``type``, target with
        | true, "verb", Some(targetId) ->
            Verb(props |> Seq.tryPick(VerbSetting.getFromText), 
                 props |> Seq.tryPick (VerbGroup.getFromText),
                 targetId)
            |> Some
        | _ ->    
            None
        

    (**Public functions**)
    let getWordTypes (patternStr : string) : Word list= 
        let mtch = SyntaxRegex().TypedMatches(patternStr)
        let res = mtch 
                  |> Seq.map(fun mtch -> 
                        match mtch.element.Value with
                        |Verb(setting, group, target) -> Verb("verb", (setting, group))
                        |Adjective(time, gender, target) -> Adjective("")
                        |_ -> Adjective("")
                  )
        []
        
   
                