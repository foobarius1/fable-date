[<RequireQualifiedAccess>]
module Date.Parse

open System

type internal ParseState =
    { LastFormatChar : Char option
      LastFormatCharCount : int
      InputRest : string
      OutputDate : DateTime }

let internal parseNext local currentFormatChar state : ParseState option =
    match state.LastFormatChar with
    | None ->
        { state with
            LastFormatChar = Some currentFormatChar
            LastFormatCharCount = 1 }
        |> Some

    | Some lastFormatChar when lastFormatChar = currentFormatChar ->
        { state with LastFormatCharCount = state.LastFormatCharCount + 1 }
        |> Some

    | Some lastFormatChar ->
        // must return Result<OutputDate * InputRest, Error> ?
        let nextDate =
            match lastFormatChar, state.LastFormatCharCount with
            | 'd', 1 -> parseDayNumeric state
            | 'd', 2 -> ()
            | 'd', 3 -> ()
            | 'd', 4 -> ()
            | 'h', 1 -> ()
            | 'h', 2 -> ()
            | 'H', 1 -> ()
            | 'H', 2 -> ()
            | 'm', 1 -> ()
            | 'm', 2 -> ()
            | 'M', 1 -> ()
            | 'M', 2 -> ()
            | 'M', 3 -> ()
            | 'M', 4 -> ()
            | 's', 1 -> ()
            | 's', 2 -> ()
            | 't', 1 -> ()
            | 't', 2 -> ()
            | 'y', 1 -> ()
            | 'y', 2 -> ()
            | 'y', 3 -> ()
            | 'y', 4 -> ()
            | 'y', 5 -> ()
            | _ -> ()

        Some state

let tryParse (local : Date.Local.Localization) (formatString : string) (input : string) : DateTime option =
    let initialState =
        { LastFormatChar = None
          LastFormatCharCount = 0
          InputRest = input
          OutputDate = DateTime.MinValue }
        |> Some

    (initialState, formatString)
    ||> Seq.fold (fun state currentFormatChar ->
        state
        |> Option.bind (parseNext local currentFormatChar)
        |> Option.map (fun state -> state.OutputDate))
