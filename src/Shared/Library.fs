module Shared.Library

module Result =
    let isOk = function
        | Ok _ -> true
        | Error _ -> false

    let getOk = function
        | Ok v -> Some v
        | Error _ -> None

    let isError = function
        | Ok _ -> false
        | Error _ -> true

    let getError = function
        | Ok _ -> None
        | Error e -> Some e

module Validators =
    open Validus
    module NL =
        let required validator =
            Validators.required validator (sprintf "%s is een verplicht veld.")

        let likert =
            Validators.Int.between 1 5 (sprintf "%s moet tussen 1 en 5 zijn.")

module ValidationResult =
    open Validus
    let getErrorList validationResult =
        validationResult
        |> Result.getError
        |> Option.map (ValidationErrors.toList)
        |> Option.defaultValue []
