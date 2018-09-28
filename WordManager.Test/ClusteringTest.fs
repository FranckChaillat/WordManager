namespace WordTaggerTest

module ClusteringTest=

    open Xunit
    open Grapher.Entities.Items
    open WordManager.Grapher.Clustering

    [<Fact>]
    let testGetTransitionMatrix() =
        let n1 = labeledNode "Node1"
        let n2 = labeledNode "Node2"
        let n3 = labeledNode "Node3"
        let n4 = labeledNode "Node4"
        let n5 = labeledNode "Node5"
        let n6 = labeledNode "Node6"

        let res = n1 --- "1to2" --> n2 --- "2to4" --> n4

        let trans = ClusteringUtils.getTransition(res)
        Assert.True(trans.[0,*] = [|0;1;0|])
        Assert.True(trans.[1,*] = [|0;0;1|])
        Assert.True(trans.[2,*] = [|0;0;0|])


    [<Fact>]
    let testGetTransitionMatrix2() =
        let n1 = labeledNode "Node1"
        let n2 = labeledNode "Node2"
        let n3 = labeledNode "Node3"
        let n4 = labeledNode "Node4"

        let res = (n1 ---"n1ton2" --> n2 ---"n2ton3" --> n3) .+  (n2 ---"n2ton4"--> n4)

        let trans = ClusteringUtils.getTransition(res)
        Assert.True(true)


