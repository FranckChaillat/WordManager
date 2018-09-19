namespace WordManager.Common.Configuration
module DataBaseConf=

    open FSharp.Configuration

    type  Neo4JSettings = AppSettings<"neo4j.config">

    type CouchBaseSettings = AppSettings<"couchbase.config">