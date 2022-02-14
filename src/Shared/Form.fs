module Shared.Form

open Validus
open Shared.Library

type Validator<'t, 'tres> = string -> 't option -> Result<'tres, ValidationErrors>

type FieldConfig<'t, 'tres> =
    { Name: string
      Validator: Validator<'t, 'tres> }

type Field<'t> =
    { Value: 't option
      Error: string option }

module Field =
    let config (name, validator) =
        { Name = name
          Validator = validator }

    let configSimple name =
        { Name = name
          Validator = (fun _ -> Ok) }

    let init () =
        { Value = None
          Error = None }

    let validate config model =
        config.Validator config.Name model.Value

    let validated config model =
        { model with
            Error =
                validate config model
                |> Result.getError
                |> Option.map (ValidationErrors.toList >> String.concat " ") }

    type Msg<'t> =
        | Update of 't
        | Validate

    let update config msg model =
        match msg with
        | Update newValue when model.Error = None ->
            { model with Value = Some newValue }
        | Update newValue ->
            validated config { model with Value = Some newValue }
        | Validate ->
            validated config model

