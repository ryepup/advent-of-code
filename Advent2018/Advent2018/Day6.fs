namespace Advent2018

open Advent2018.Utils
open System
open System.IO
open System.Text

module Day6 =

    type Point = {X:int; Y:int}
    let origin = {X=0; Y=0}
    type Destination = {Point:Point; Closest:Point option}

    let parsePoint line =
        match line with
            | Regex @"(\d+), (\d+)" [Integer x; Integer y] -> {X = x; Y = y}
            | _ -> failwith "could not parse"

    let getBounds (points:seq<Point>) : Point =
        Seq.fold
            (fun acc p ->
                match p with
                    | {X=x;Y=y} when x > acc.X && y <= acc.Y ->
                        {acc with X=x}
                    | {X=x;Y=y} when x <= acc.X && y > acc.Y ->
                        {acc with Y=y}
                    | {X=x;Y=y} when x > acc.X && y > acc.Y -> p
                    | _ -> acc)
            {X= 0; Y= 0}
            points

    let manhattanDist (a:Point) (b:Point) =
        abs (a.X - b.X) + abs (a.Y - b.Y)

    let findClosest (points:seq<Point>) (a:Point) =
        points
        |> Seq.map (fun p -> (p, (manhattanDist a p)))
        |> Seq.sortBy (fun (_, n) -> n)
        |> Seq.take 2
        |> Seq.toList
        |> function
            | [(p1,n1); (_,n2)] when n1 < n2 -> Some p1
            | _ -> None

    let findBorderPointsWrong1 (points:seq<Point>) =
        let upper = getBounds points
        let lower = points |> Seq.minBy (manhattanDist origin)

        points
        |> Seq.filter (fun p ->
            Seq.contains p.X [lower.X; upper.X]
            || Seq.contains p.Y [lower.Y; upper.Y])
        |> Set.ofSeq

    let findBorderPointsWrong2 (points:seq<Point>) =
        let hasPointNW t =
            points |> Seq.exists (fun p ->
                p.Y < t.Y && p.X < t.X)
        let hasPointNE t =
            points |> Seq.exists (fun p ->
                p.Y < t.Y && p.X > t.X)
        let hasPointSE t =
            points |> Seq.exists (fun p ->
                p.Y > t.Y && p.X > t.X)
        let hasPointSW t =
            points |> Seq.exists (fun p ->
                p.Y > t.Y && p.X < t.X)
        let isInTheMiddle t =
            hasPointNW t && hasPointNE t && hasPointSE t && hasPointSW t

        points
        |> Seq.filter (isInTheMiddle >> not)
        |> Set.ofSeq

    let findBorderPoints (points:seq<Point>) =
        let bounds = getBounds points

        // loop over the borders and see who's closest
        // there's gotta be a better way to do this
        [
            Seq.allPairs [0..bounds.X] [0;bounds.Y]
            Seq.allPairs [0;bounds.X] [0..bounds.Y]
        ]
        |> Seq.concat
        |> Seq.map (fun (x,y) -> {X=x; Y=y})
        |> Seq.choose (findClosest points)
        |> Set.ofSeq

    let makeGrid (points:seq<Point>) =
        let bounds = getBounds points
        Seq.allPairs [0..bounds.X] [0..bounds.Y]
        |> Seq.map (fun (x,y) -> {X=x;Y=y})
        |> Seq.map (fun p ->
            {
                Point=p
                Closest=(findClosest points p)
            })

    let render points dests =
        let upper =
            dests
            |> Seq.map (fun d -> d.Point)
            |> Seq.maxBy (manhattanDist origin)
        let lookup =
            points |> Seq.mapi (fun i p -> (p, char (int 'a' + i)))
            |> Map.ofSeq
        let map = Array2D.create (upper.X+1) (upper.Y+1) '?'
        let mark p c =
            Array2D.set map p.X p.Y c

        dests |> Seq.iter (fun d ->
            match d.Closest with
                | Some p -> mark d.Point (Map.find p lookup)
                | None -> mark d.Point '.')
        lookup |> Map.iter (fun p c -> mark p (Char.ToUpper c))

        let sb = new StringBuilder()
        sb.AppendLine("") |> ignore
        for y in [0..upper.Y] do
            for x in [0..upper.X] do
                sb.Append(map.[x,y]) |> ignore
            sb.AppendLine("") |> ignore

        sb.ToString()

    let solve1 (points:seq<Point>) =
        let borders = findBorderPoints points
        makeGrid points
        |> Seq.choose (fun d -> d.Closest)
        |> Seq.groupBy id
        |> Seq.filter ((fun (c,_) -> c) >> borders.Contains >> not)
        |> Seq.map (fun (c,ps) -> (c, Seq.length ps))
        |> Seq.maxBy (fun (_,n) -> n)

    let points = lazy (File.ReadAllLines("Advent2018/Advent2018/day6.txt") |> Seq.map parsePoint )
