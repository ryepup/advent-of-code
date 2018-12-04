namespace Advent2018
open System.IO
open System
open System.Drawing
open System.Text.RegularExpressions
open Microsoft.FSharp.Collections

module Day3 =

    type Plan = { Id: string; Rect: Rectangle; Raw: string}

    let parseRect (m:Match) =
        [2;3;4;5]
        |> List.map ((fun i -> m.Groups.[i].Value) >> Int32.Parse)
        |> function
            | [left; top; width; height] -> Rectangle.FromLTRB(left, top, left+width, top+height)
            | _ -> failwith "could not parse rect"


    let parsePlan line =
        // #1379 @ 542,442: 22x15
        match Regex.Match(line, @"^#(\d+) @ (\d+),(\d+): (\d+)x(\d+)$") with
            | m when m.Success -> {
                    Raw = line
                    Id = m.Groups.[1].Value;
                    Rect = parseRect m
                }
            | _ -> failwith "could not parse plan"

    let parsePlans lines =
        Seq.map parsePlan lines

    let plans =
        lazy(
        "Advent2018/Advent2018/day3.txt"
        |> File.ReadAllLines
        |> parsePlans
        )

    let findOverlaps plan plans =
        plans
        |> Seq.filter (fun x -> not (x = plan))
        |> Seq.map (fun x -> (Rectangle.Intersect(plan.Rect, x.Rect)))
        |> Seq.filter (fun x -> not x.IsEmpty && x.Width > 0 && x.Height > 0)

    let toBits (rect:Rectangle) =
        Array2D.create rect.Height rect.Width true

    let array2DCollect source =
        seq {
            for x in 0 .. Array2D.length1 source - 1 do
                for y in 0 .. Array2D.length2 source - 1 do
                    yield source.[x, y]
        }

    let findArea plans =
        plans
        |> Seq.collect (fun plan -> findOverlaps plan plans)
        |> Seq.fold
            (fun acc rect ->
                Array2D.blit (toBits rect) 0 0 acc rect.Top rect.Left rect.Height rect.Width
                acc
                )
            (Array2D.create 1000 1000 false)
        |> array2DCollect
        |> Seq.sumBy Convert.ToInt32

    let findDisjoint plans =
        plans
        |> Seq.filter (fun plan -> findOverlaps plan plans |> Seq.isEmpty)
        |> Seq.head

    let findDisjointId plans =
        (findDisjoint plans).Id
