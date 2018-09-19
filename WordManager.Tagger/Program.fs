open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open WordTagger.WordTaggerService

//let app =
//  choose
//    [ GET >=> choose
//        [ pathScan "/wordtracking/%d/%s" (fun (depth, word)-> startWordTag depth word)
//          path "/goodbye" >=> OK "Good bye GET" ] ]

//[<EntryPoint>]
//let main argv = 
//    startWebServer defaultConfig app
//    0 
