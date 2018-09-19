namespace WordTaggerTest
module SyntacticParserTest =
    
    open Xunit
    open WordManager.Tagger.Model.FrenchModel
    open WordTagger
    open WordManager.Tagger.Parser

    [<Fact>]
    let TestSyntacticParsing() =
        let words = [PersonalPronoun("Le", (Some(Sing), Some(Male))); Noun("chat", (Some(Sing), Some(Male)));Verb("mange", (Some(VerbSetting.Present), Some(VerbGroup.First)))]
        let example = "{1:NOUN[SING;MALE]->6};{2:VERB[PRESENT;FIRST]->9}"
        let parseResult = Parser.getWordTypes example
        Assert.True(fst(Word.format(parseResult.Item(0)).Item(1)) = "noun")
        Assert.True(fst(Word.format(parseResult.Item(0)).Item(1)) = "noun")
        Assert.True(fst(Word.format(parseResult.Item(1)).Item(1)) = "verb")
        

    //[<Fact>]
    //let TestWildcardParsing() = 
    //    let example = "{1:NOUN[SING;MALE]->6};{2:VERB[PRESENT;FIRST]->9}"
    //    let result = Wildcard.parse(example)
    //    Assert.True(result.IsSome)
    //    Assert.True((result.Value.items |> Seq.length) = 2)