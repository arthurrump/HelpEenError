module Shared.Form

open Validus
open Shared.Library

type Validator<'t, 'tres> = string -> 't option -> Result<'tres, ValidationErrors>

[<CustomEquality; NoComparison>]
type Field<'t, 'tres when 't : equality> =
    { Name: string
      Value: 't option
      Error: string option
      Validator: Validator<'t, 'tres> }

    override this.GetHashCode () =
        hash (this.Name, this.Value, this.Error)

    override this.Equals (that: obj) =
        match that with
        | :? Field<'t, 'tres> as that ->
            this.Name = that.Name &&
            this.Value = that.Value &&
            this.Error = that.Error
        | _ ->
            false

module Field =
    let init (name, validator) =
        { Name = name
          Value = None
          Error = None
          Validator = validator }

    let initSimple name =
        { Name = name
          Value = None
          Error = None
          Validator = (fun _ -> Ok) }

    let validate field =
        field.Validator field.Name field.Value

    let validated field =
        { field with
            Error =
                validate field
                |> Result.getError
                |> Option.map (ValidationErrors.toList >> String.concat " ") }

    type Msg<'t> =
        | Update of 't
        | Validate

    let update msg model =
        match msg with
        | Update newValue when model.Error = None ->
            { model with Value = Some newValue }
        | Update newValue ->
            validated { model with Value = Some newValue }
        | Validate ->
            validated model

