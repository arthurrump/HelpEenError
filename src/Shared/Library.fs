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

    let required validator message field value =
        match value with
        | Some v -> validator field v
        | None -> Error (ValidationErrors.create field [ message field ])

    module NL =
        let required validator =
            required validator (sprintf "%s is een verplicht veld.")

        let likert =
            Validators.Int.between 1 5 (sprintf "%s moet tussen 1 en 5 zijn.")

    module String =
        let contains (substr: string) msg =
            Validator.create msg (fun (str: string) -> str.Contains substr)

module ValidationResult =
    open Validus
    let getErrorList validationResult =
        validationResult
        |> Result.getError
        |> Option.map (ValidationErrors.toList)
        |> Option.defaultValue []
