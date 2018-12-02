namespace Advent2018
open System.IO
open System

module Day2 =
    let ids =
        "Advent2018/Advent2018/day2.txt"
        |> File.ReadAllLines

    let hasRepeats n input =
        input
        |> Seq.countBy id
        |> Seq.exists (fun (_, count) -> count = n)

    let hasDouble input =
        hasRepeats 2 input
    let hasTriple input =
        hasRepeats 3 input

    let checksum ids =
        ids
        |> Seq.fold (fun acc input ->
            Seq.zip acc [hasDouble; hasTriple]
            |> Seq.map
                (fun (value, pred) -> if pred input then value + 1 else value)
            ) (Seq.ofList [0; 0])
        |> Seq.reduce ( * )

    let hasOneCharDifferent a b =
        Seq.map2
            (fun charA charB -> if charA = charB then 0 else 1)
            a b
        |> Seq.sum
        |> function
            | 1 -> true
            | _ -> false

    let findCommonId (a:string) (b:string) =
        Seq.zip a b
        |> Seq.filter (fun (a, b) -> a = b)
        |> Seq.map (fun (a,_) -> a)
        |> String.Concat

    let findCode ids =
        let findMatch id i =
            ids
            |> Seq.skip i
            |> Seq.tryFind (hasOneCharDifferent id)

        ids
        |> Seq.mapi (fun i id ->
            match findMatch id i with
                | None -> None
                | Some value -> Some (id, value)
        )
        |> Seq.choose id // drop the Nones
        |> Seq.tryHead
        |> function
            | None -> failwith "no ids found"
            | Some (a,b) -> findCommonId a b


//findCode ids
// guess 1, wrong: mphcuasvrnjzzakbgdtqeoylva
// guess 2, right: mphcuasvrnjzzkbgdtqeoylva
