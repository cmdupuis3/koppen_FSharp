
namespace Koppen

open System.Threading.Tasks

[<Measure>] type C
[<Measure>] type mm
[<Measure>] type m
[<Measure>] type s
[<Measure>] type day

module Koppen =
    /// Convert meters to millimeters
    let m2mm (m: float<m>) = m * 1000.0<mm/m>
    /// Convert millimeters to meters
    let mm2m (mm: float<mm>) = mm / 1000.0<mm/m>
    /// Convert seconds to days
    let s2day (s: float<s>) = s / 84600.0<s/day>
    /// Convert days to seconds
    let day2s (day: float<day>) = day * 84600.0<s/day>

    let private daysInMonths (daysInFebruary: float<day>) =
        [|31.0<day>; daysInFebruary; 31.0<day>; 30.0<day>; 31.0<day>; 30.0<day>; 31.0<day>; 31.0<day>; 30.0<day>; 31.0<day>; 30.0<day>; 31.0<day>|]

    let private annualMeanOf (vals: float<_>[]) (daysInFebruary: float<day>) =
        (vals, (daysInMonths daysInFebruary))
        ||> Array.fold2 (fun acc elem1 elem2 -> acc + elem1*elem2) 0.0<_>
        |> fun x -> x / Array.sum (daysInMonths daysInFebruary)

    type Zone =
        | Af | Am | Aw
        | BWh | BWk | BSh | BSk
        | Csa | Csb | Csc
        | Cwa | Cwb | Cwc
        | Cfa | Cfb | Cfc
        | Dsa | Dsb | Dsc | Dsd
        | Dwa | Dwb | Dwc | Dwd
        | Dfa | Dfb | Dfc | Dfd
        | ET | EF

    type Hemisphere = | Northern | Southern

    type Climate (daysInFebruary: float<day>, hemisphere: Hemisphere, temperature: float<C>[], precipitation: float<mm>[]) =
        member this.DaysInFebruary = daysInFebruary
        member this.Hemisphere = hemisphere
        member this.Temperature = temperature
        member this.Precipitation = precipitation

    let private winter = function | Northern -> [|1..3|]   | Southern -> [|7..9|]
    let private spring = function | Northern -> [|4..6|]   | Southern -> [|10..12|]
    let private summer = function | Northern -> [|7..9|]   | Southern -> [|1..3|]
    let private autumn = function | Northern -> [|10..12|] | Southern -> [|4..6|]
    let private springAndSummer = fun h -> Array.append (spring h) (summer h)
    let private autumnAndWinter = fun h -> Array.append (autumn h) (winter h)

    type private DesertPrecip = | Arid | SemiArid | Moist
    type private TemperatureMajor = | Tropical | Temperate | Continental | Polar
    type private Precip = | YearRound | Mediterranean | Monsoon
    type private TemperatureMinor =  | Hot | Warm | Cold | Severe

    let private major (clim: Climate) =
        let tMin = Array.min clim.Temperature
        if tMin > 18.0<C> then Tropical else
            if tMin > -3.0<C> then Temperate else
                if Array.max clim.Temperature < 10.0<C> then Polar else
                    Continental

    let private minor (clim: Climate) =
        let numWarmMonths = clim.Temperature |> Array.filter (fun x -> x > 10.0<C>) |> Array.length
        if numWarmMonths >= 4 then
            if Array.max clim.Temperature > 22.0<C> then Hot else Warm
        else
            if Array.min clim.Temperature < -38.0<C> then Severe else Cold

    let private precip (clim: Climate) =
        let summerMax = springAndSummer clim.Hemisphere |> Array.map (fun i -> clim.Precipitation.[i-1]) |> Array.max
        let winterMin = autumnAndWinter clim.Hemisphere |> Array.map (fun i -> clim.Precipitation.[i-1]) |> Array.min
        if summerMax > 10.0 * winterMin then Monsoon else
            let winterMax = autumnAndWinter clim.Hemisphere |> Array.map (fun i -> clim.Precipitation.[i-1]) |> Array.max
            let summerMin = springAndSummer clim.Hemisphere |> Array.map (fun i -> clim.Precipitation.[i-1]) |> Array.min
            if winterMax > 3.0 * summerMin && summerMin < 30.0<mm> then Mediterranean else
                YearRound

    let private arid (clim: Climate) =
        let precipRatio = (springAndSummer clim.Hemisphere |> Array.sumBy (fun i -> clim.Precipitation.[i-1])) / Array.sum clim.Precipitation
        let precipOffset = (10.0<mm/C> * annualMeanOf clim.Temperature clim.DaysInFebruary) + if(precipRatio >= 0.7) then 140.0<mm> else if(precipRatio >= 0.3) then 70.0<mm> else 0.0<mm>
        let precipTotal = Array.sum clim.Precipitation
        if precipTotal < precipOffset then Arid else
            if precipTotal >= precipOffset && precipTotal <= 2.0 * precipOffset then SemiArid else
                Moist

    let private (|MatchArid|_|) (clim: Climate) =
        match arid clim, major clim with
        | Moist,    _                         -> None
        | Arid,     (Tropical    | Temperate) -> Some BWh
        | Arid,     (Continental | Polar)     -> Some BWk
        | SemiArid, (Tropical    | Temperate) -> Some BSh
        | SemiArid, (Continental | Polar)     -> Some BSk

    let private MatchTropical (clim: Climate) =
        if Array.forall (fun x -> x >= 60.0<mm>) clim.Precipitation then Af else
            let a = 100.0<mm> - ((Array.sum clim.Precipitation) / 25.0)
            if Array.min clim.Precipitation >= a then Am else Aw

    let private MatchPolar (clim: Climate) =
        if Array.max clim.Temperature < 0.0<C> then EF else ET

    let private MatchTemperate (clim: Climate) =
        match precip clim, minor clim with
        | _,           Severe -> failwith "C climates cannot have severe winters"
        | Monsoon,       Hot  -> Cwa
        | Monsoon,       Warm -> Cwb
        | Monsoon,       Cold -> Cwc
        | Mediterranean, Hot  -> Csa
        | Mediterranean, Warm -> Csb
        | Mediterranean, Cold -> Csc
        | YearRound,     Hot  -> Cfa
        | YearRound,     Warm -> Cfb
        | YearRound,     Cold -> Cfc

    let private MatchContinental (clim: Climate) =
        match precip clim, minor clim with
        | Monsoon,       Hot    -> Dwa
        | Monsoon,       Warm   -> Dwb
        | Monsoon,       Cold   -> Dwc
        | Monsoon,       Severe -> Dwd
        | Mediterranean, Hot    -> Dsa
        | Mediterranean, Warm   -> Dsb
        | Mediterranean, Cold   -> Dsc
        | Mediterranean, Severe -> Dsd
        | YearRound,     Hot    -> Dfa
        | YearRound,     Warm   -> Dfb
        | YearRound,     Cold   -> Dfc
        | YearRound,     Severe -> Dfd

    let Zones (clim: Climate) =
        match clim with
        | MatchArid k -> k // must be first
        | _ ->
            match major clim with
            | Tropical    -> clim |> MatchTropical
            | Temperate   -> clim |> MatchTemperate
            | Continental -> clim |> MatchContinental
            | Polar       -> clim |> MatchPolar


    let private allA = [Af; Am; Aw]

    let private allC =
        [
            Csa; Csb; Csc;
            Cfa; Cfb; Cfc;
            Cwa; Cwb; Cwc
        ]

    let private allD =
        [
            Dsa; Dsb; Dsc; Dsd;
            Dfa; Dfb; Dfc; Dfd;
            Dwa; Dwb; Dwc; Dwd;
        ]

    let ZoneList = allA @ [BWh; BWk; BSh; BSk] @ allC @ allD @ [ET; EF]

    /// List of all valid Koppen zone transitions, given as a zone and its neighbors.
    let TransitionList =
        [
            Af, [Am; Aw; BSh] @ allC;
            Am, [Af; Aw; BSh] @ allC;
            Aw, [Af; Am; BSh] @ allC;

            BWh, [BSh; BWk];
            BWk, [BSk; BWh];
            BSh, allA @ [BWh; BSk] @ allC;
            BSk, [BSh; BWk] @ allD @ [ET];

            Csa, allA @ [BSh; Dsa; Csb; Cfa];
            Csb, allA @ [BSh; Dsb; Csa; Csc; Cfb];
            Csc, allA @ [BSh; Dsc; Csb; Cfc]            @ [ET];
            Cfa, allA @ [BSh; Dfa; Csa; Cwa; Cfb];
            Cfb, allA @ [BSh; Dfb; Csb; Cwb; Cfa; Cfc];
            Cfc, allA @ [BSh; Dfc; Csc; Cwc; Cfb]       @ [ET];
            Cwa, allA @ [BSh; Dwa; Cwb; Cfa];
            Cwb, allA @ [BSh; Dwb; Cfb; Cwa; Cwc];
            Cwc, allA @ [BSh; Dwc; Cfc; Cwb]            @ [ET];

            Dsa, [BSk; Csa; Dsb; Dfa]
            Dsb, [BSk; Csb; Dsa; Dsc; Dfb];
            Dsc, [BSk; Csc; Dsb; Dsd; Dfc]              @ [ET];
            Dsd, [BSk;      Dsc; Dfd]                   @ [ET];
            Dfa, [BSk; Cfa; Dsa; Dwa; Dfb];
            Dfb, [BSk; Cfb; Dsb; Dwb; Dfa; Dfc];
            Dfc, [BSk; Cfc; Dsc; Dwc; Dfb; Dfd]         @ [ET];
            Dfd, [BSk;      Dsd; Dwd; Dfc]              @ [ET];
            Dwa, [BSk; Cwa; Dwb; Dfa];
            Dwb, [BSk; Cwb; Dfb; Dwa; Dwc];
            Dwc, [BSk; Cwc; Dfc; Dwb; Dwd]              @ [ET];
            Dwd, [BSk;      Dfd; Dwc]                   @ [ET];

            ET, [Csc; Cfc; Cwc; Dsc; Dsd; Dfc; Dfd; Dwc; Dwd; EF];
            EF, [ET]
        ]

    let Transitions (zone: Zone) =
        TransitionList
        |> List.find (fun x -> fst x = zone)
        |> snd


    type private Halo(north: 'a, east: 'a, south: 'a, west: 'a) =
        member this.North = north
        member this.East = east
        member this.South = south
        member this.West = west

    /// Halo for a regular grid wrapped horizontally
    let private halo i j (grid: 'a list list) =

        let east =
            if i = grid.Length - 1 then 0 else i + 1
            |> fun x -> grid.[x].[j]
        let west =
            if i = 0 then grid.Length - 1 else i - 1
            |> fun x -> grid.[x].[j]
        let north = if j = grid.[i].Length - 1 then None else grid.[i].[j+1]
        let south = if j = 0 then None else grid.[i].[j-1]

        Halo(north, east, south, west)

    let private countTransitions (grid: 'a list list) =
        let transitions =
            List.init grid.Length (fun i ->
                List.init grid.[i].Length (fun j ->
                    match grid.[i].[j] with
                    | None   -> []
                    | Some k ->
                        grid
                        |> halo i j
                        |> fun x -> [x.North; x.East; x.South; x.West]
                        |> List.filter (fun x -> x.IsSome)
                        |> List.map (Option.get >> (fun x -> k, x))
                        |> List.filter (fun x -> fst x <> snd x)
                )
            )
            |> List.reduce (@)
            |> List.reduce (@)

        // If there is a valid transition, return the transition indices
        let transitionIndex zone1 zone2 =
            let first =
                TransitionList
                |> List.findIndex (fun x -> fst x = zone1)
            let second =
                TransitionList.[first]
                |> snd
                |> List.tryFindIndex (fun x -> x = zone2)
            match second with
            | None -> None
            | Some j -> Some (first, j)

        // Initialize transition buckets to zero
        let buckets =
            Array.init TransitionList.Length (fun i ->
                Array.init (TransitionList.[i] |> snd |> List.length) (fun j ->
                    0
                )
            )

        let coords =
            transitions
            |> List.map (fun x -> transitionIndex (fst x) (snd x))
            |> List.choose id

        for i, j in coords do
            buckets.[i].[j] <- buckets.[i].[j] + 1
        buckets

    let private graphWeights (cost: int -> float) (grid: 'a list list) =
        countTransitions grid
        |> Array.toList
        |> List.map (fun x ->
            x
            |> Array.toList
            |> List.map cost
        )

    let Graph (cost: int -> float) (grid: 'a list list) =
        (TransitionList |> List.map snd, graphWeights cost grid)
        ||> List.map2 List.zip
        |> fun x -> (TransitionList |> List.map fst, x)
        ||> List.zip

    let private graphKeys (cost: int -> float) (grid: 'a list list) =
        Graph cost grid
        |> List.map (fun x ->
            snd x
            |> List.map (fun y ->
                fst x, fst y
            )
        )

    let GraphAsMap (cost: int -> float) (grid: 'a list list) =
        (graphKeys cost grid, graphWeights cost grid)
        ||> List.map2 List.zip
        |> List.reduce (@)
        |> Map.ofList

    //Dijkstra's algorithm: Nigel Galloway, August 5th., 2018
    // from https://rosettacode.org/wiki/Dijkstra%27s_algorithm#F.23
    [<CustomEquality;CustomComparison>]
    type private Dijkstra<'N, 'G when 'G:comparison>
        = {toN: 'N; cost: Option<'G>; fromN: 'N}
            override g.Equals n =
                match n with
                | :? Dijkstra<'N,'G> as n -> n.cost = g.cost
                | _ -> false
            override g.GetHashCode() = hash g.cost
            interface System.IComparable with
                member n.CompareTo g =
                    match g with
                    | :? Dijkstra<'N,'G> as n when n.cost = None -> (-1)
                    | :? Dijkstra<'N,'G>      when n.cost = None -> 1
                    | :? Dijkstra<'N,'G> as g                    -> compare n.cost g.cost
                    | _-> invalidArg "n" "expecting type Dijkstra<'N,'G>"

    // from https://rosettacode.org/wiki/Dijkstra%27s_algorithm#F.23
    let inline private Dijkstra N G y =
        let rec fN l f =
            if List.isEmpty l then f else
                let n = List.min l
                if n.cost=None then f else
                fN (l |> List.choose (fun n' ->
                    if n'.toN=n.toN then None else
                    match n.cost, n'.cost, Map.tryFind (n.toN,n'.toN) G with
                    | Some g, None,    Some wg                  -> Some {toN = n'.toN; cost = Some(g+wg); fromN = n.toN}
                    | Some g, Some g', Some wg when g + wg < g' -> Some {toN = n'.toN; cost = Some(g+wg); fromN = n.toN}
                    | _                                         -> Some n'))
                    ((n.fromN, n.toN) :: f)

        let r = fN (N |> List.map (fun n -> {toN = n; cost = (Map.tryFind (y,n) G); fromN = y})) []
        (fun n ->
            let rec fN z l =
                match List.tryFind (fun (_,g) -> g = z) r with
                | Some (n', g') when y = n'-> Some (n' :: g' :: l)
                | Some (n', g')            -> fN n' (g' :: l)
                | _                        -> None
            fN n [])

    let private dictionary func =
        //let keys = graphKeys cost grid |> List.reduce (@)
        let keys =
            List.init ZoneList.Length (fun i ->
                List.init ZoneList.Length (fun j ->
                    printf "%i    %i\n" i j
                    (ZoneList.[i], ZoneList.[j])))
            |> List.reduce (@)
        printfn "keys length: %i\n" keys.Length

        let timer = System.Diagnostics.Stopwatch.StartNew()
        timer.Start()
        let vals =
            keys
            |> List.map (fun x -> func (fst x) (snd x))
        timer.Stop()
        printfn "%i s\n" timer.ElapsedMilliseconds

        printfn "%s\n" "here2"
        (keys, vals)
        ||> List.zip


    let Paths (cost: int -> float) (grid: 'a list list) =
        Dijkstra ZoneList (GraphAsMap cost grid)

    let PathDictionary (cost: int -> float) (grid: 'a list list) =
        Paths cost grid |> dictionary



    let Distance (cost: int -> float) (grid: 'a list list) (zone1: Zone) (zone2: Zone)
        (pathDictionary: ((Zone * Zone) * Zone list Option) list) =
        let map = GraphAsMap cost grid

        pathDictionary
        |> List.find (fun ((x, y), z) -> (x, y) = (zone1, zone2))
        |> snd
        |> function
            | None -> infinity
            | Some path ->
                List.init (path.Length-1) (fun i ->
                    map |> Map.find (path.[i], path.[i+1])
                )
                |> List.reduce (+)

    let DistanceDictionary (cost: int -> float) (grid: 'a list list) =
        let pathDictionary = PathDictionary cost grid
        (fun a b -> Distance cost grid a b pathDictionary)
        |> dictionary

    let Difference (cost: int -> float) (grid1: Zone Option list list) (grid2: Zone Option list list) =
        let distanceDictionary = DistanceDictionary cost grid1
        List.init grid1.Length (fun i ->
            List.init grid2.Length (fun j ->
                printf "%i    %i\n" i j
                match grid1.[i].[j], grid2.[i].[j] with
                | None, None -> None
                | Some zone1, Some zone2 ->
                    distanceDictionary
                    |> List.find (fun ((x, y), z) -> (x, y) = (zone1, zone2))
                    |> snd
                    |> Some
                | _, _ -> failwith "Grids don't match"
            )
        )
