namespace WordTaggerTest

module ClusteringTest=

    open Xunit
    open Grapher.Entities.Items
    open WordManager.Grapher.Clustering
    open Grapher.Entities

    [<Fact>]
    let testGetTransitionMatrix() =
        let n1 = labeledNode "Node1"
        let n2 = labeledNode "Node2"
        let n4 = labeledNode "Node4"

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
        Assert.True(trans.[0,*]  = [|0;1;0;0|])
        Assert.True(trans.[1,*]  = [|0;0;1;1|])
        Assert.True(trans.[2,*]  = [|0;0;0;0|])
        Assert.True(trans.[3,*]  = [|0;0;0;0|])


    [<Fact>]
    let testMatrixNormalization() = 
        let n1 = labeledNode "Node1"
        let n2 = labeledNode "Node2"
        let n3 = labeledNode "Node3"
        let n4 = labeledNode "Node4"

        let res = (n1 ---"n1ton2" --> n2 ---"n2ton3" --> n3) .+  (n2 ---"n2ton4"--> n4)

        let trans = ClusteringUtils.getTransition(res)
        let normalized = ClusteringUtils.normalize(trans)
        Assert.True(normalized.[0,*] = [|0.0; 0.25; 0.0; 0.0|])
        Assert.True(normalized.[1,*] = [|0.0; 0.0; 0.25; 0.25|])
        Assert.True(normalized.[2,*] = [|0.0; 0.0; 0.0; 0.0|])
        Assert.True(normalized.[3,*] = [|0.0; 0.0; 0.0; 0.0|])


    [<Fact>]
    let testMatrixExpansion() =
        let mat = [| [|1.0;2.0;3.0|]; [|4.0; 5.0; 6.0|]; [|7.0; 8.0 ; 9.0;|] |]
        let twoDimArr = Array2D.zeroCreate 3 3
        mat |> Array.iteri(fun i e -> e |> Array.iteri(fun j v -> Array2D.set twoDimArr i j v))
        let expanded = MclOptions.create(1,1)
                       |> Option.map(fun opt -> ClusteringUtils.expand(opt)(twoDimArr))
        Assert.True(expanded.Value.[0,*] = [|30.0; 36.0; 42.0|])
        Assert.True(expanded.Value.[1,*] = [|66.0; 81.0; 96.0|])
        Assert.True(expanded.Value.[2,*] = [|102.0; 126.0; 150.0|])


    [<Fact>]
    let testMatrixExpansionWithFloat() =
        let mat = [| [|0.23;0.11;0.7|]; [|0.99; 1.0; 1.0|]; [|0.0; 0.86 ; 0.5|] |]
        let twoDimArr = Array2D.zeroCreate 3 3
        mat |> Array.iteri(fun i e -> e |> Array.iteri(fun j v -> Array2D.set twoDimArr i j v))
        let expanded = MclOptions.create(1,1)
                        |> Option.map(fun opt -> ClusteringUtils.expand(opt)(twoDimArr))
        Assert.True(expanded.IsSome)                       
        Assert.True(expanded.Value.[0,*] = [|0.1618; 0.7373; 0.621|])


    [<Fact>]
    let testMatrixExpansionWithNegativeExpansion() = 
        Assert.True(MclOptions.create(-1, 2).IsNone)
        Assert.True(MclOptions.create(1,-2).IsNone)
        Assert.True(MclOptions.create(1,1).IsSome)