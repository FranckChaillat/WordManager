namespace WordManager.Common.DataProvider
module CouchBaseProvider=

    open Couchbase
    open Couchbase.Configuration.Client
    open Couchbase.Authentication
    open WordManager.Common.Configuration
    open WordManager.Common.Entities
    open WordManager.Framework.Tools.Operators
    open System
    
    let private getConfiguration =
        let serverList = new System.Collections.Generic.List<Uri>()
        serverList.Add(new Uri(DataBaseConf.CouchBaseSettings.WordpatternCluster))
        new ClientConfiguration(Servers = serverList)


    let private cluster =
        let cluster = new Cluster(getConfiguration)
        let authenticator = new PasswordAuthenticator(DataBaseConf.CouchBaseSettings.Login, DataBaseConf.CouchBaseSettings.Password);
        cluster.Authenticate(authenticator)
        cluster


    let private retrieveDocument<'RetrieveType>(id: string) =
        use b = cluster.OpenBucket(DataBaseConf.CouchBaseSettings.WordpatternBucketName)
        let result = b.GetDocument<'RetrieveType>(id)
        result.Success ?= (Some(result.Content), None)


    let private insertDocument<'InsertType> (doc: CouchDocument<'InsertType>) = 
        use b = cluster.OpenBucket(DataBaseConf.CouchBaseSettings.WordpatternBucketName)
        let document = new Document<'InsertType>(Content = doc.content, Id = doc.id)
        let upsert = b.Upsert(document)
        upsert.Message


    let couchBaseProvider<'T, 'U> = 
         GenericProvider.GenericProvider(insertDocument<'T>, insertDocument<'T>, retrieveDocument<'U>)
