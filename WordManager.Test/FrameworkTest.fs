namespace WordTaggerTest
module FrameworkTest =
    
    open Xunit
    open WordManager.Framework.Tools.ListExtensions
    open WordManager.Framework.Tools   
   

    [<Fact>]
    let testListReplacement() =
        let t = ["franck"; "morgan"; "adrien"; "karl"; "melanie"]
        let res = List.replace (fun (x: string) -> x.StartsWith("m")) "na" t
        let nth = List.nth res
        Assert.Equal("na", (nth 1))
        Assert.Equal("na", (nth 4))

    [<Fact>]
    let testListSpliting() = 
        let t = ["franck"; "morgan"; "adrien"; "karl"; "melanie"]
        let res = List.split (fun x -> x = "adrien") t
        Assert.Equal(2, res.Length)
        Assert.True(["franck"; "morgan"] = res.Head)


    [<Fact>]
    let testSeqTryItem() = 
        let t = ["franck"; "morgan"; "adrien"; "karl"; "melanie"] |> List.toSeq
        let res = Seq.tryItem 0 t
        Assert.Equal("franck", res.Value)

    [<Fact>]
    let testSeqTryItem2() = 
        let t = ["franck"; "morgan"; "adrien"; "karl"; "melanie"] |> List.toSeq
        let res = Seq.tryItem 2 t
        Assert.Equal("adrien", res.Value)

    [<Fact>]
    let testSeqTryItemFail() = 
        let t = ["franck"; "morgan"; "adrien"; "karl"; "melanie"] |> List.toSeq
        let res = Seq.tryItem 10 t
        Assert.True(res.IsNone)

        