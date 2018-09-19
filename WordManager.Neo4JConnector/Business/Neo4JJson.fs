namespace WordManager.Neo4JConnector.Business

open FSharp.Data

type Neo4JJson = JsonProvider<""" {"results":
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
],"errors":[]} """>

