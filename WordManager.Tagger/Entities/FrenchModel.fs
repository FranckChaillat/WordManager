namespace Model
module FrenchModel =

    (**Noun or pronoun gender**)
    type Gender =
        |Male
        |Female
    with static member format gender =
            match gender with
            |Male -> "gender", "male"
            |Female -> "gender", "female"

         static member isValid (genderProperty: string) =
            genderProperty.ToLowerInvariant().Trim()
            |> function | "male" | "female" -> true 
                        | _ -> false
         
         static member getFromText (genderProperty : string) =
             match genderProperty.ToLowerInvariant().Trim() with
             |"female" -> Some Male
             |"male" -> Some Female
             |_ -> None
    
    (**Verb time**)
    type Time =
        |Sing
        |Plur
    with static member format time =
            match time with
            |Sing -> "time", "sing"
            |Plur -> "time", "plur"
        
         static member isValid (timeProperty: string) =
             timeProperty.ToLowerInvariant().Trim()
             |> function | ("plur" | "sing") -> true 
                         | _ -> false

         static member getFromText (timeProperty : string) =
             match timeProperty.ToLowerInvariant().Trim() with
             |"sing" | "singular" -> Some Sing
             |"plur" | "plurial" -> Some Plur
             |_ -> None

    (**Verb group**)
    type VerbGroup = 
        |First
        |Second
        |Third
    with static member format group =
            match group with
            |First -> "group", "first"
            |Second -> "group", "second" 
            |Third -> "group", "third"

         static member isValid (verbProperty: string) =
             verbProperty.ToLowerInvariant().Trim()
             |> function | "first" | "second" | "third" -> true 
                         | _ -> false

         static member getFromText(verbProperty : string) =
             match verbProperty.ToLowerInvariant().Trim() with
             |"first" | "fst"-> Some First
             |"second" | "snd" -> Some Second
             |"third" | "trd" -> Some Third
             |_ -> None

    (**Verb setting**)
    type VerbSetting = 
        |Infinitive
        |PerfectTense
        |Present
        |Future
    with static member format setting = 
            match setting with
            |Infinitive -> "setting", "infinitive"
            |PerfectTense -> "setting", "perfectTense"
            |Present -> "setting", "present"
            |Future -> "setting", "future"

         static member isValid (verbSetting: string) =
             verbSetting.ToLowerInvariant().Trim()
             |> function | "infinitive" | "perfecttense" | "present" | "future" -> true 
                         | _ -> false
         
         static member getFromText (verbSetting : string) =
            match verbSetting.ToLowerInvariant().Trim() with
            |"infinitive" -> Some Infinitive
            |"perfecttense" -> Some PerfectTense
            |"present" -> Some Present
            |"future" -> Some Future
            |_ -> None
    

    type VerbModel = VerbSetting option * VerbGroup option
    type NounModel = Time option * Gender option

    //TODO: rendre générique
    type WordModel = 
        |Verb of VerbModel
        |Noun of NounModel
        |Adjective
        |PersonalPronoun of NounModel
    with static member format(wordType) = 
            match wordType with
            |Verb(setting, group) ->
                [
                "type", "verb" :> obj
                setting |> Option.map(VerbSetting.format) |> Option.defaultValue ("","") |> fun (k,v) -> k, v:>obj
                group |> Option.map(VerbGroup.format) |> Option.defaultValue ("", "") |> fun (k,v) -> k, v:>obj
                ]
            |Noun(time, gender) | PersonalPronoun(time, gender) -> 
                [
                 "type", ("noun":>obj)
                 time |> Option.map (Time.format) |> Option.defaultValue ("", "") |> fun (k,v) -> k, (v:>obj)
                 gender |> Option.map (Gender.format) |> Option.defaultValue ("", "") |> fun (k,v) -> k, (v:>obj)
                ]
            |Adjective -> 
                [ "type", "adjective":>obj ]


    type Word = 
        |Verb of string * VerbModel
        |Noun of string * NounModel
        |Adjective of string
        |PersonalPronoun of string * NounModel
    with static member format (wordtype) =
            match wordtype with
            |Verb(s, verbModel) ->
                ("label", s :> obj) :: WordModel.format(WordModel.Verb(verbModel))
            |Noun(s, nounModel) 
            |PersonalPronoun(s, nounModel) -> 
                ("label", (s:>obj)) :: WordModel.format(WordModel.Noun(nounModel))
            |Adjective(s) -> 
                ("label", s:> obj) :: WordModel.format(WordModel.Adjective)
    

    type FunctionType = 
        | Subject
        | SubjectComplement
        | VerbComplement 
        | Modifier


    type SyntacticFunction = FunctionType * (Word option)


    type WordTag = {
        id : int
        text : string
        word : Word list
        linkedTo : string list
    }

