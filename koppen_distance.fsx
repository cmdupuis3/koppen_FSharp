
#load @"koppen.fs"
open Koppen



type Halo(north: 'a Option, east: 'a Option, south: 'a Option, west: 'a Option) =
    member this.North = north
    member this.East = east
    member this.South = south
    member this.West = west

/// Halo for a regular grid wrapped horizontally
let halo i j (grid: 'a Option list list) =

    let east =
        if i = grid.Length - 1 then 0 else i + 1
        |> fun x -> grid.[x].[j]
    let west =
        if i = 0 then grid.Length - 1 else i - 1
        |> fun x -> grid.[x].[j]
    let north = if j = grid.[i].Length - 1 then None else grid.[i].[j+1]
    let south = if j = 0 then None else grid.[i].[j-1]

    Halo(north, east, south, west)



let countTransitions (grid: 'a Option list list) =


    List.init grid.Length (fun i ->
        List.init grid.[i].Length (fun j ->
            match grid.[i].[j] with
            | Some k ->
                let transitions =
                    grid
                    |> halo i j
                    |> fun x -> [x.North; x.East; x.South; x.West]
                    |> List.filter (fun x -> x.IsSome)
                    |> List.map Option.get

                Koppen.Transitions k
                |>

            | None   -> []




        )
    )
