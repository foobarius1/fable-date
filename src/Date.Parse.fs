[<RequireQualifiedAccess>]
module Date.Parse

open System

type internal AmPm =
    | Am
    | Pm

type internal ParseState =
    { LastFormatChar : Char option
      LastFormatCharCount : int
      InputRest : string
      OutputDate : DateTime
      AmPm : AmPm option }

let internal stringSkip count (input : string) =
    Seq.skip count input
    |> String.Concat

let internal parseNumeric toDate state =
    state.InputRest
    |> Seq.takeWhile Char.IsDigit
    |> Seq.toList
    |> function
    | [] -> None
    | digits ->
        let ok, i = String.Concat digits |> Int32.TryParse
        if not ok then None
        else
            { state with
                OutputDate = toDate state.OutputDate i
                InputRest = stringSkip digits.Length state.InputRest }
            |> Some

let internal parseSecondNumeric = parseNumeric (fun date i -> (i - date.Second) |> float |> date.AddSeconds)
let internal parseMinuteNumeric = parseNumeric (fun date i -> (i - date.Minute) |> float |> date.AddMinutes)
let internal parseHourNumeric   = parseNumeric (fun date i -> (i - date.Hour)   |> float |> date.AddHours)
let internal parseDayNumeric    = parseNumeric (fun date i -> (i - date.Day)    |> float |> date.AddDays)
let internal parseMonthNumeric  = parseNumeric (fun date i -> (i - date.Month)  |> date.AddMonths)
let internal parseYearNumeric   = parseNumeric (fun date i -> (i - date.Year)   |> date.AddYears)

//let internal parseDayAbbrev (local : Date.Local.Localization) state: ParseState option =
//    let abbreviatedDays =
//        [ local.Date.AbbreviatedDays.Monday
//          local.Date.AbbreviatedDays.Tuesday
//          local.Date.AbbreviatedDays.Wednesday
//          local.Date.AbbreviatedDays.Thursday
//          local.Date.AbbreviatedDays.Friday
//          local.Date.AbbreviatedDays.Saturday
//          local.Date.AbbreviatedDays.Sunday ]
//
//    failwith "not implemented"


let internal parseMonthWithList months state =
    months
    |> List.tryFindIndex (fun month -> state.InputRest.StartsWith month)
    |> Option.map (fun index ->
        let monthStrLen = months.[index].Length
        let monthNumeric = index + 1
        let nextDate =
            (monthNumeric - state.OutputDate.Month)
            |> state.OutputDate.AddMonths
        { state with
            OutputDate = nextDate
            InputRest = stringSkip monthStrLen state.InputRest })

let internal parseMonthAbbrev (local: Date.Local.Localization) =
    let months =
        [ local.Date.AbbreviatedMonths.January
          local.Date.AbbreviatedMonths.February
          local.Date.AbbreviatedMonths.March
          local.Date.AbbreviatedMonths.April
          local.Date.AbbreviatedMonths.May
          local.Date.AbbreviatedMonths.June
          local.Date.AbbreviatedMonths.July
          local.Date.AbbreviatedMonths.August
          local.Date.AbbreviatedMonths.September
          local.Date.AbbreviatedMonths.October
          local.Date.AbbreviatedMonths.November
          local.Date.AbbreviatedMonths.December ]
    parseMonthWithList months

let internal parseMonthLong (local: Date.Local.Localization) =
    let months =
        [ local.Date.Months.January
          local.Date.Months.February
          local.Date.Months.March
          local.Date.Months.April
          local.Date.Months.May
          local.Date.Months.June
          local.Date.Months.July
          local.Date.Months.August
          local.Date.Months.September
          local.Date.Months.October
          local.Date.Months.November
          local.Date.Months.December ]
    parseMonthWithList months

let internal parseAmPm skipLen (amPmTransform: string -> string) (local: Date.Local.Localization) state =
    match local.Time with
    | { AM = ""; PM = "" } -> None
    | { AM = am; PM = _  } when state.InputRest.StartsWith (amPmTransform am) ->
        Some Am
    | { AM = _;  PM = pm } when state.InputRest.StartsWith (amPmTransform pm) ->
        Some Pm
    | { AM = _; PM = _ } -> None
    |> Option.map (fun amPm ->
        { state with
            AmPm = Some amPm
            InputRest = stringSkip skipLen state.InputRest })

let internal parseAmPmShort = parseAmPm 1 (fun s -> s.Substring(0,1))
let internal parseAmPmLong  = parseAmPm 2 id

let internal updateAmPm state =
    match state.AmPm with
    | Some Am when state.OutputDate.Hour >= 12 -> None
    | Some Pm when state.OutputDate.Hour < 12 ->
        { state with
            OutputDate = state.OutputDate.AddHours 12. }
        |> Some
    | Some Pm
    | Some Am
    | None -> Some state

let internal parseNext local currentFormatChar state =
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
        let parseFn =
            match lastFormatChar, state.LastFormatCharCount with
            | 'd', 1
            | 'd', 2 -> parseDayNumeric
            //| 'd', 3 -> parseDayAbbrev local state
            //| 'd', 4 -> ()
            | 'h', 1
            | 'h', 2
            | 'H', 1
            | 'H', 2 -> parseHourNumeric
            | 'm', 1
            | 'm', 2 -> parseMinuteNumeric
            | 'M', 1
            | 'M', 2 -> parseMonthNumeric
            | 'M', 3 -> parseMonthAbbrev local
            | 'M', 4 -> parseMonthLong local
            | 's', 1
            | 's', 2 -> parseSecondNumeric
            | 't', 1 -> parseAmPmShort local
            | 't', 2 -> parseAmPmLong local
            | 'y', 1
            | 'y', 2
            | 'y', 3
            | 'y', 4
            | 'y', 5 -> parseYearNumeric
            | _ -> Some
        parseFn state
        |> Option.bind updateAmPm

let tryParse local (formatString : string) input : DateTime option =
    let initialState =
        { LastFormatChar = None
          LastFormatCharCount = 0
          InputRest = input
          OutputDate = DateTime.MinValue
          AmPm = None }
        |> Some

    (initialState, formatString)
    ||> Seq.fold (fun state currentFormatChar -> Option.bind (parseNext local currentFormatChar) state)
    |> Option.map (fun state -> state.OutputDate)

let inline cannotParse formatString input =
    raise (FormatException(sprintf "could not parse \"%s\" with format \"%s\"" input formatString))

let parse local formatString input =
    let dateTimeOption = tryParse local formatString input
    match dateTimeOption with
    | Some d -> d
    | None -> cannotParse formatString input
