
namespace Koppen

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

    let CountTransitions (grid: 'a list list) =
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
