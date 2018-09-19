namespace WordTaggerTest
module SyntacticParserTest =
    
    open Xunit
    open Model.FrenchModel
    open WordTagger
    open Wildcard

    //[<Fact>]
    //let TestSyntacticParsing() =
    //    let words = [PersonalPronoun("Le", Sing, Male); Noun("chat", Sing, Male);Verb("mange", VerbSetting.Present, VerbGroup.First]
    //    let example = "{1:NOUN[SING;MALE]->6};{2:VERB[PRESENT;FIRST]->9}"
    //    let parseResult = Parser.getWordTypes example
    //    Assert.True(fst(Word.format(parseResult.Item(0)).Item(1)) = "noun")
    //    Assert.True(fst(Word.format(parseResult.Item(0)).Item(1)) = "noun")
    //    Assert.True(fst(Word.format(parseResult.Item(1)).Item(1)) = "verb")
        

    //[<Fact>]
    //let TestWildcardParsing() = 
    //    let example = "{1:NOUN[SING;MALE]->6};{2:VERB[PRESENT;FIRST]->9}"
    //    let result = Wildcard.parse(example)
    //    Assert.True(result.IsSome)
    //    Assert.True((result.Value.items |> Seq.length) = 2)