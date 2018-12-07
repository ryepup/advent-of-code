namespace Advent2018

open System.Text.RegularExpressions

/// Common helpers, mostly pulled from
/// https://docs.microsoft.com/en-us/dotnet/fsharp/language-reference/
module Utils =

    let (|Integer|_|) (str: string) =
       let mutable intvalue = 0
       if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
       else None

    let (|Regex|_|) pattern input =
        match Regex.Match(input, pattern) with
            | m when m.Success ->
                Some(List.tail [for g in m.Groups -> g.Value])
            | _ -> None
