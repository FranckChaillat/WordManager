namespace WordManager.Common.DataProviders
module CouchBaseProvider =
    
    open Couchbase.Core
    open Couchbase.Configuration
    open Couchbase.Configuration.Client
    open WordManager.Common.Configuration
    open System
    open Couchbase

    let private configuration =
        let mutable configuration = new ClientConfiguration()
        let servers = new System.Collections.Generic.List<Uri>()
        servers.Add(new Uri(DataBaseConf.CouchBaseSettings.WordpatternCluster))
        configuration.Servers <- servers
        configuration
    
    ClusterHelper.Initialize(configuration)
    ClusterHelper.GetBucket("")
