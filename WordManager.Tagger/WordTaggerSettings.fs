namespace WordTagger

module SettingsEcosystem =

    open FSharp.Configuration

    type Settings = AppSettings<"app.config">

    