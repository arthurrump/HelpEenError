module Elmish.Persistance

open Browser.Types
open Browser.WebStorage
open Fable.Core.JsInterop
open Fable.SimpleJson
open Elmish

module Program =
    let inline withPersistance (storage: Storage) (storageKey: string) (program: Program<'arg option,'model,'msg,'view>) =
        Program.map
            // init
            (fun initFunc ->
                let state = storage.getItem storageKey
                if isNullOrUndefined state
                then initFunc
                else fun _ -> initFunc (Some (Json.parseAs<'arg> state)))
            // update
            id
            // view
            id
            // setState
            (fun setState ->
                fun state dispatch ->
                    setState state dispatch
                    storage.setItem (storageKey, Json.serialize<'model> state))
            // subscribe
            id
            program

    let inline withLocalPersistance storageKey program = withPersistance localStorage storageKey program
    let inline withSessionPersistance storageKey program = withPersistance sessionStorage storageKey program
