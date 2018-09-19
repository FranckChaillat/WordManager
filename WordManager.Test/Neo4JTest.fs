namespace WordTaggerTest
module Neo4JTest =

      open Xunit
      open Grapher.Business.PathFinding
      open Grapher.Business.Utils
      open Grapher.Entities.Items
      open WordManager.Common.DataProvider.Neo4JProvider


      
      [<Fact>]
      let relationCreationTest() = 
        let n = node "Node1" []
        let m = node "Node2" []
        
        let res = n --- "Relation1" --> m
        Assert.True(res.Length = 2)
        Assert.True((Graph.nth res 1) |> function Some m -> true | _ -> false)
        let (n, e) = res.Head.outputNodes.Head
        Assert.True(e.direction = LeftToRight)
        Assert.True(e.name = "Relation1")

      [<Fact>]
      let relationLeftToRight() =
        let n1 = node "Node1" []
        let n2 = node "Node2" []
        let n3 = node "Node3" []
        let relation = n1 --- "" --> n2
        let relation2 = relation --- "" --> n3
        Assert.Equal(3, relation2.Length)
        Assert.True(n3 == (Graph.nth relation2 2).Value)
        Assert.Equal(Some "Node2", (Graph.nth relation2 1).Value.label)
     
      [<Fact>]
      let relationRightToLeft() = 
        let n1 = node "Node1" []
        let n2 = node "Node2" []

        let rel =  n2 <-- (n1 --- "Relation1")
        Assert.Equal(2, rel.Length)
        Assert.Equal(RightToLeft, snd(rel.Head.outputNodes.Head).direction)

      [<Fact>]
      let relationBothDirections() = 
        let n1 = node "Node1" []
        let n2 = node "Node2" []
        let n3 = node "Node3" []

        let rel =  n2 <-- (n1 --- "Relation1")
        Assert.Equal(2, rel.Length)
        Assert.Equal(RightToLeft, snd(rel.Head.outputNodes.Head).direction)


      [<Fact>]
      let testEdgeToString() = 
        let n1 = node "Node1" []
        let n2 = node "Node2" []

        let g = n1 --- "edgeName" --> n2
        let edge = (Graph.nth g 0).Value.outputNodes.Head
        let str = snd(edge) |> edgeToString "rel"
        Assert.Equal("-[rel:edgeName]->", str)


      [<Fact>]
      let testNodeToString() = 
        let n1 = node "Node1" [("name", "franck" :> obj); ("age", 24 :> obj)]
        let res = (nodesToString [n1]).Head
        Assert.Equal("(n0 : Node1{name : \'franck\',age : 24})", res)


      [<Fact>]
      let testNodeEqulity() = 
        let n1 = node "Node1" [("name", "franck" :> obj); ("age", "24" :> obj)]
        let n2 = node "Node1" [("name", "franck" :> obj); ("age", "24" :> obj)]
        Assert.True(n1==n2)

      [<Fact>]
      let testBranchingHead() = 
        let n1 = labeledNode "Node1"
        let n2 = labeledNode "Node2"
        let n3 = labeledNode "Node3"
        let n4 = labeledNode "Node4" 

        let g1 = n1 --- "rel" --> n2
        let g2 = n1 --- "rel2" --> n3
        
        let res = branchHead [Graph.nodes g1; Graph.nodes g2] n1 n4 "test"
        Assert.Equal(2, res.Value.Length)
        Assert.True(res.Value.Head.Head == n4)
        Assert.True(res.Value.Tail.Head.Head == n4)


      [<Fact>]
      let testBranchingHead2() = 
        let n1 = labeledNode "Node1"
        let n2 = labeledNode "Node2"
        let n3 = labeledNode "Node3"
        let n4 = labeledNode "Node4" 

        let g1 = n3 --- "rel0" --> n1 --- "rel" --> n2
        
        let res = branchHead [Graph.nodes g1] n1 n4 "test"
        Assert.Equal(1, res.Value.Length)
        Assert.True(res.Value.Head.Head == n4)

        
      [<Fact>]
      let testBranchingTail() = 
        let n1 = labeledNode "Node1"
        let n2 = labeledNode "Node2"
        let n3 = labeledNode "Node3"
        let n4 = labeledNode "Node4" 

        let g1 = n1 --- "rel" --> n2
        let g2 = n1 --- "rel2" --> n3
        
        let res = branchTail [Graph.nodes g1; Graph.nodes g2] n2 n4 "test" []
        Assert.True(res.IsSome)
        Assert.Equal(1, res.Value.Length)
        Assert.True((List.item 2 res.Value.Head ) == n4)


      [<Fact>]
      let testBranchingTail2() = 
        let n1 = labeledNode "Node1"
        let n2 = labeledNode "Node2"
        let n3 = labeledNode "Node3"
        let n4 = labeledNode "Node4" 

        let g1 = n1 --- "rel" --> n2
        let g2 = n1 --- "rel2" --> n3
        
        let res = branchTail [Graph.nodes g1; Graph.nodes g2] n4 n4 "test" []
        Assert.True(res.IsNone)


      [<Fact>]
      let testNodeLookup() =
        let n1 = labeledNode "Node1"
        let n2 = labeledNode "Node2"
        let n3 = labeledNode "Node3"
        let n4 = labeledNode "Node4"

        let res = nodeLookup [n1; n2; n3; n4] n2
        Assert.Equal(3, res.Length)


      [<Fact>]
      let testPathSpliting() = 
         let n1 = labeledNode "Node1"
         let n2 = labeledNode "Node2"
         let n3 = labeledNode "Node3"
         let n4 = labeledNode "Node4"
         
         let res = splitPaths [[n1; n2; n3; n4]; [n1; n3; n2; n4]] n2
         
         Assert.True(res.Head = [n2; n4])
         Assert.True(res.Tail.Head = [n2; n3; n4])
         

      [<Fact>]
      let testPathSpliting2() = 
         let n1 = labeledNode "Node1"
         let n2 = labeledNode "Node2"
         let n3 = labeledNode "Node3"
         let n4 = labeledNode "Node4"
         
         let res = splitPaths [[n1; n2; n3; n4]; [n1; n3; n4]] n2
         Assert.True(res.Head = [n2; n3; n4])
         
         Assert.True(true)



      [<Fact>]
      let testPathMinus() =
        let n1 = labeledNode "Node1"
        let n2 = labeledNode "Node2"
        let n3 = labeledNode "Node3"
        let n4 = labeledNode "Node4"
        
        let path1 = [n1; n2; n3]
        let path2 = [n4; n2; n3]
        let minus = minus path1 path2

        Assert.Equal(n4, minus.Head.Head)
        Assert.Equal(n2, List.item 1 minus.Head)

      [<Fact>]
      let testPathMinus2() =
        let n1 = labeledNode "Node1"
        let n2 = labeledNode "Node2"
        let n3 = labeledNode "Node3"
        let n4 = labeledNode "Node4"
        let n5 = labeledNode "Node5"
        
        let path1 = [n4; n2; n3]
        let path2 = [n1; n2; n5]
        let minus = minus path1 path2

        Assert.True(minus.Head = [n2; n5])
        Assert.True(minus.Tail.Head =  [n1; n2])




      [<Fact>]
      let testPathMinus3() =
        let n1 = labeledNode "Node1"
        let n2 = labeledNode "Node2"
        let n3 = labeledNode "Node3"
        let n4 = labeledNode "Node4"
        let n5 = labeledNode "Node5"
        let n6 = labeledNode "Node6"
        
        let path1 = [n1; n2; n3; n6]
        let path2 = [n4; n2; n3; n5]
        let minus = minus path1 path2

        Assert.True(minus.Head = [n3; n5])
        Assert.True(minus.Tail.Head =  [n4; n2])


      [<Fact>]
      let testPathMinus4() =
        let n2 = labeledNode "Node2"
        let n3 = labeledNode "Node3"
        let n4 = labeledNode "Node4"
        let n5 = labeledNode "Node5"

        
        let path1 = [n5; n2; n3]
        let path2 = [n5; n2; n4]
        let minus = minus path1 path2

        Assert.True(minus.Head = [n2; n4])


      [<Fact>]
      let testPathFinding() = 
         let n1 = labeledNode "Node1"
         let n2 = labeledNode "Node2"
         let n3 = labeledNode "Node3"
         let n4 = labeledNode "Node4"

         let res = (n1 ---"n1ton2" --> n2 ---"n2ton3" --> n3) .+  (n2 ---"n2ton4"--> n4)
         let paths = Graph.getFullPaths res
         
         let firstPathIndex = List.nth paths.Head
         let secondPathIndex = List.nth paths.Tail.Head

         Assert.True(paths.Length = 2)
         Assert.True(paths.Head.Length = 3)
         Assert.True(paths.Tail.Head.Length = 3)
         Assert.True(firstPathIndex(0) == n1 && firstPathIndex(1) == n2 && firstPathIndex(2) == n4)


      [<Fact>]
      let testPathFinding2() = 
         let n1 = labeledNode "Node1"
         let n2 = labeledNode "Node2"
         let n3 = labeledNode "Node3"
         let n4 = labeledNode "Node4"
         let n5 = labeledNode "Node5"
         let n6 = labeledNode "Node6"
         
         let res = n1 --- "1to2" --> n2 --- "2to4" --> n4
                    .+ (n5 --- "5to1" --> n2 ---"2to3" --> n3)
                    .+ (n6 --- "6to5" --> n5)
         let paths = Graph.getFullPaths res
         Assert.True(paths.Length = 4)
           


      [<Fact>]
      let testGraphConcat() = 
         let n1 = labeledNode "Node1"
         let n2 = labeledNode "Node2"
         let n3 = labeledNode "Node3"
         let n4 = labeledNode "Node4"

         let res = (n1 ---"n1ton2" --> n2 ---"n2ton3" --> n3) .+  (n2 ---"n2ton4"--> n4)
         let nodes = Graph.nodes res
         Assert.Equal(4, res.Length)
         Assert.True(List.exists (fun x -> x == n4) nodes)
         Assert.True(nodes |> List.find(fun x -> x == n2) |> (fun x -> x.outputNodes.Length = 2))
       
         
      [<Fact>]
      let testGraphToString() = 
         let n1 = node "Node1" [("name", "franck" :> obj); ("age", 24 :> obj)]
         let n2 = labeledNode "Node2"
         let n3 = labeledNode "Node3"
         let n4 = labeledNode "Node4"
         let n5 = labeledNode "Node5"
          
         let res = n1 --- "1to2" --> n2 --- "2to4" --> n4
                 .+ (n5 --- "5to2" --> n2 ---"2to3" --> n3)
         
         let graphStr = graphToString(res)
         let result = ["(n4 : Node2)";
                       "(n3 : Node5)-[e30:5to2]->(n4)-[e41:2to3]->(n6 : Node3)";
                       "(n0 : Node1{name : 'franck',age : 24})-[e00:1to2]->(n4)"
                       "(n4)-[e40:2to4]->(n2 : Node4)"]
                        
         Assert.True(graphStr.Equals(result))
      

      [<Fact>]
      let testQueryBuilding() =
         let n1 = labeledNode "Node1"
         let n2 = labeledNode "Node2"

         let res = n1 ---"n1ton2" --> n2
         let query = (formatMerge res).Head
         Assert.Equal("MERGE (n0 : Node1)-[e00:n1ton2]->(n1 : Node2)", query)


      [<Fact>]
      let testQueryBuilding2() =
         let n1 = labeledNode "Node1"
         let n2 = labeledNode "Node2"
         let n3 = labeledNode "Node3"
         let n4 = labeledNode "Node4"

         let res = n1 --- "n1to2" --> n2 --- "n2to4" --> n4
         let query = (formatMerge res).Head
         Assert.Equal("MERGE (n0 : Node1)-[e00:n1to2]->(n1 : Node2)-[e11:n2to4]->(n2 : Node4)", query)
      

      [<Fact>]
      let testQueryBuilding3() =
         let n1 = node "Node1" [("name", "franck" :> obj); ("age", 24 :> obj)]
         let n2 = labeledNode "Node2"
         let n3 = labeledNode "Node3"
         let n4 = labeledNode "Node4"

         let res = n1 --- "n1to2" --> n2
                   .+ (n3 --- "n3to2" --> n2)

         let mergeStatement = String.concat "," (formatMerge res)
         Assert.Equal("MERGE (n3 : Node2),MERGE (n0 : Node1{name : 'franck',age : 24})-[e00:n1to2]->(n3),MERGE (n2 : Node3)-[e20:n3to2]->(n3)", mergeStatement)
         

      [<Fact>]
      let testGraphConcatenation() =

         let network = labeledNode("Node2") ---"rel1" --> labeledNode("Node3") 
                       .+ (labeledNode("Node2") --- "rel2" --> labeledNode "Node4")

         let first = (Graph.nth network 0).Value
         let scnd = (Graph.nth network 1).Value
         let thrd = (Graph.nth network 2).Value

         let node2 = Graph.nodes (network) |> List.find(fun (x : Node) -> x.label = Some "Node2")
         let node3 = Graph.nodes (network) |> List.find(fun (x : Node) -> x.label = Some "Node3")
         let node4 = Graph.nodes (network) |> List.find(fun (x : Node) -> x.label = Some "Node4")

         Assert.True(node2.outputNodes |> List.map(fun (n,_) -> n.label.Value) = ["Node3";"Node4"])
         Assert.True(node3.outputNodes |> List.map(fun (n,_) -> n.label.Value) = [])
         Assert.True(node4.outputNodes |> List.map(fun (n,_) -> n.label.Value) = [])

         let links = scnd.outputNodes |> List.map(fun (n, _) -> n.id)

         Assert.True(first.id = 3 && first.label = Some "Node4")
         Assert.True(thrd.id = 1 && thrd.label = Some "Node3")
         Assert.True(scnd.id = 2 && links = [1;3])
         


      [<Fact>]
      let testGetGraphPaths() =
         let n1 = node "Node1" [("name", "franck" :> obj); ("age", 24 :> obj)]
         let n2 = labeledNode "Node2"
         let n3 = labeledNode "Node3"
         let n4 = labeledNode "Node4"
         let n5 = labeledNode "Node5"
          
         let res = n1 --- "n1to2" --> n2 --- "n2to4" --> n4
                 .+ (n5 --- "n5to1" --> n2 ---"n2to3" --> n3)

         let paths = getGraphPaths res
         Assert.True(paths.Length = 3)
         let p1 = paths.[0] |> List.map(fun x -> x.id)
         let p2 = paths.[1] |> List.map(fun x -> x.id)
         let p3 = paths.[2] |> List.map(fun x -> x.id)
         Assert.True([3;4;6] = p1)
         Assert.True([0;4] = p2)
         Assert.True([4;2] = p3)

        
      [<Fact>]
      let testGraphDeserialization() =
        let jsonRes = """ {"results":
            [
	            {"columns":["n0","rel","n1"],
	             "data":
	 	            [
	 		            {"graph": {
	 			            "nodes":[
	 				            {"id":"28","labels":["test1"],"properties":{}},
	 				            {"id":"29","labels":["test2"],"properties":{}}
	 			            ],
	 			             "relationships":[ {"id":"4","type":"know","startNode":"28","endNode":"29","properties":{}}]
	 			            }
	 		            }
	 	            ]
	            }
            ],"errors":[]} """
        let graph = jsonToGraph jsonRes
        Assert.True(graph.IsSome)
        let g = graph.Value
        let nodes = Graph.nodes g
        Assert.True(2 = nodes.Length)
        Assert.Equal(Some("test1"), (nodes.Item 0).label)
        Assert.Equal(Some("test2"), (nodes.Item 1).label)
        let n, e = nodes.Head.outputNodes.Head
        Assert.True(e.name = "know")
        Assert.Equal("know", e.name)
    

      [<Fact>]
      let testGraphDeserialization2() = 
        let jsonRes = """ {"results":
            [
	            {"columns":["n0","rel","n1","rel2","n2"],
	             "data":
	 	            [
	 		            {"graph": {
	 			            "nodes":[
	 				            {"id":"28","labels":["test1"],"properties":{}},
	 				            {"id":"29","labels":["test2"],"properties":{}},
	 				            {"id":"30","labels":["test3"],"properties":{}}

	 			            ],
	 			             "relationships":[ {"id":"4","type":"know","startNode":"28","endNode":"29","properties":{}},
	 			             				   {"id":"5","type":"workwith","startNode":"28","endNode":"30","properties":{}}]
	 			            }
	 		            }
	 	            ]
	            }
            ],"errors":[]} """
        let graph = jsonToGraph jsonRes
        Assert.True(graph.IsSome)
        let g = graph.Value
        let nodes = Graph.nodes g
        Assert.Equal(Some("test3"), (List.item 0 nodes).label)
        Assert.Equal(Some("test1"), (List.item 1 nodes).label)
        let (n,e) :: ((n2,e2):: []) = (List.item 1 nodes).outputNodes
        Assert.Equal(Some("test2"), n.label)
        Assert.Equal("know", e.name)
        Assert.Equal(Some("test3"), n2.label)
        Assert.Equal("workwith", e2.name)

    
      [<Fact>]
      let testStatementsFormating() =
         let n1 = node "Node1" [("name", "franck" :> obj); ("age", 24 :> obj)]
         let n2 = labeledNode "Node2"
         let n3 = labeledNode "Node3"
         let n4 = labeledNode "Node4"
         let n5 = labeledNode "Node5"
          
         let graph = n1 --- "n1to2" --> n2 --- "n2to4" --> n4
         let result = WordManager.Common.DataProvider.Neo4JProvider.merge graph
         Assert.True(true)

    
        
        
      [<Fact>]
      let testStatementsFormating2() =
          
         //let graph = (labeledNode "Test1") ---"know" --> (labeledNode "Test2") --- "know" --> (labeledNode "Test3")
         //let result = Neo4JConnector.Commands.``match`` graph
         let res = WordManager.Common.DataProvider.Neo4JHttpProvider.commit """{
          "statements" : [ {
            "statement" : "match (n0)-[rel:know]->(n1) return n0,rel,n1",
            "resultDataContents" : [ "graph" ]
          } ]
        }"""
         Assert.True(true)