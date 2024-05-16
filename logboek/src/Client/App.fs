module App

open Elmish
open Elmish.Persistance
open Elmish.React

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram Index.init Index.update Index.view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactSynchronous "elmish-app"
|> Program.withLocalPersistance "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.runWith None
