
namespace Koppen

[<Measure>] type C
[<Measure>] type m
[<Measure>] type mm
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

    let private winter = function | Northern -> [1..3]   | Southern -> [7..9]
    let private spring = function | Northern -> [4..6]   | Southern -> [10..12]
    let private summer = function | Northern -> [7..9]   | Southern -> [1..3]
    let private autumn = function | Northern -> [10..12] | Southern -> [4..6]

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
        let numWarmMonths = clim.Temperature |> Array.fold (fun acc elem -> if elem > 10.0<C> then acc + 1 else acc) 0
        if numWarmMonths >= 4 then 
            if Array.max clim.Temperature > 22.0<C> then Hot else Warm
        else
            if Array.min clim.Temperature < -38.0<C> then Severe else Cold

    let private precip (clim: Climate) = 
        let summerMax = List.toArray (summer clim.Hemisphere) |> Array.fold (fun acc elem -> if acc > clim.Precipitation.[elem] then acc else clim.Precipitation.[elem]) 0.0<mm>
        let winterMin = List.toArray (winter clim.Hemisphere) |> Array.fold (fun acc elem -> if acc < clim.Precipitation.[elem] then acc else clim.Precipitation.[elem]) 9999.0<mm>
        if summerMax > 10.0 * winterMin then Monsoon else
            let winterMax = List.toArray (winter clim.Hemisphere) |> Array.fold (fun acc elem -> if acc > clim.Precipitation.[elem] then acc else clim.Precipitation.[elem]) 0.0<mm>
            let summerMin = List.toArray (summer clim.Hemisphere) |> Array.fold (fun acc elem -> if acc < clim.Precipitation.[elem] then acc else clim.Precipitation.[elem]) 9999.0<mm>
            if winterMax > 3.0 * summerMin && summerMin < 30.0<mm> then Mediterranean else
                YearRound

    let private arid (clim: Climate) = 
        let springAndSummer = List.append (spring clim.Hemisphere) (summer clim.Hemisphere)
        let precipRatio = (List.toArray springAndSummer |> Array.fold (fun acc elem -> acc + clim.Precipitation.[elem]) 0.0<mm>) / Array.sum clim.Precipitation
        let precipOffset = (10.0<mm/C> * annualMeanOf clim.Temperature clim.DaysInFebruary) + if(precipRatio >= 0.7) then 140.0<mm> else if(precipRatio >= 0.3) then 70.0<mm> else 0.0<mm>
        let precipTotal = Array.sum clim.Precipitation
        if precipTotal < precipOffset then Arid else
            if precipTotal >= precipOffset && precipTotal <= 2.0 * precipOffset then SemiArid else
                Moist

    let private (|MatchArid|_|) (clim: Climate) = 
        match arid clim, major clim with
        | Moist,    _                      -> None
        | Arid,     (Tropical | Temperate) -> Some BWh
        | Arid,     (Continental | Polar)  -> Some BWk
        | SemiArid, (Tropical | Temperate) -> Some BSh
        | SemiArid, (Continental | Polar)  -> Some BSk

    let private (|MatchTropical|_|) (clim: Climate) = 
        if Array.forall (fun x -> x >= 60.0<mm>) clim.Precipitation then Some Af else
            let a = 100.0<mm> - ((Array.sum clim.Precipitation) / 25.0)
            if Array.min clim.Precipitation >= a then Some Am else Some Aw

    let private (|MatchPolar|_|) (clim: Climate) = 
        if Array.max clim.Temperature < 0.0<C> then Some EF else Some ET

    let private (|MatchTemperate|_|) (clim: Climate) =
        match precip clim, minor clim with
        | _,           Severe -> failwith "C climates cannot have severe winters"
        | Monsoon,       Hot  -> Some Cwa
        | Monsoon,       Warm -> Some Cwb
        | Monsoon,       Cold -> Some Cwc
        | Mediterranean, Hot  -> Some Csa
        | Mediterranean, Warm -> Some Csb
        | Mediterranean, Cold -> Some Csc
        | YearRound,     Hot  -> Some Cfa
        | YearRound,     Warm -> Some Cfb
        | YearRound,     Cold -> Some Cfc

    let private (|MatchContinental|_|) (clim: Climate) =
        match precip clim, minor clim with
        | Monsoon,       Hot    -> Some Dwa
        | Monsoon,       Warm   -> Some Dwb
        | Monsoon,       Cold   -> Some Dwc
        | Monsoon,       Severe -> Some Dwd
        | Mediterranean, Hot    -> Some Dsa
        | Mediterranean, Warm   -> Some Dsb
        | Mediterranean, Cold   -> Some Dsc
        | Mediterranean, Severe -> Some Dsd
        | YearRound,     Hot    -> Some Dfa
        | YearRound,     Warm   -> Some Dfb
        | YearRound,     Cold   -> Some Dfc
        | YearRound,     Severe -> Some Dfd

    let Zones (clim: Climate) =
        match clim with
        | MatchArid k -> Some k // must be first
        | _ -> 
            match major clim with
            | Tropical    -> clim |> function | MatchTropical    k -> Some k | _ -> None
            | Temperate   -> clim |> function | MatchTemperate   k -> Some k | _ -> None
            | Continental -> clim |> function | MatchContinental k -> Some k | _ -> None
            | Polar       -> clim |> function | MatchPolar       k -> Some k | _ -> None
