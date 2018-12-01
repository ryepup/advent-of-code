module Day1
open System.IO

let day1 =
    Array.sumBy System.Int32.Parse (File.ReadAllLines("Advent2018/Advent2018/day1-frequency.txt"))
