
#I @"/home/c1d/.nuget/packages/fsharp.data/3.3.2/lib/net45/"
#r @"FSharp.Data.dll"
#load @"koppen.fs"
open Koppen
open System.IO
open FSharp.Data


type KoppenFile = CsvProvider<"/local/home/landproj/CMIP6_historical/tmp/100_135.csv", 
                              HasHeaders = true, 
                              Schema = "float, string, float, float">
let file = KoppenFile.Load("/local/home/landproj/CMIP6_historical/tmp/100_135.csv")

let imax = 360
let jmax = 180

let readKoppen imax jmax = 

    let rec readKoppenCol j imax jmax = 
        let rec readKoppenRow i j jmax =
            let fname = String.concat "" ["/local/home/landproj/CMIP6_historical/tmp/"; string i; "_"; string j; ".csv"]
            if not(File.Exists fname) then
                if i = imax then [None] else None :: (readKoppenRow (i+1) j jmax)
            else
                let file = KoppenFile.Load("/local/home/landproj/CMIP6_historical/tmp/100_135.csv")
                    
                let daysInFebruary = (file.Rows |> Seq.head).DaysInFebruary * 1.0<day>
                let hemisphere = (file.Rows |> Seq.head).Hemisphere |> function | "Northern" -> Koppen.Northern | "Southern" -> Koppen.Southern
                let temperature =   file.Rows |> Seq.map (fun x -> x.Temperature)   |> Seq.map (float >> (*) 1.0<C>) |> Seq.toArray
                let precipitation = file.Rows |> Seq.map (fun x -> x.Precipitation) |> Seq.map (float >> (*) 1.0<mm>) |> Seq.toArray

                Koppen.Climate (daysInFebruary, hemisphere, temperature, precipitation)
                |> Koppen.Zones 
                |> Option.get
                |> fun x -> if i = imax then [Some x] else Some x :: (readKoppenRow (i+1) j jmax)
        let row = readKoppenRow 1 j jmax
        if j = jmax then [row] else row :: (readKoppenCol (j+1) imax jmax)

    readKoppenCol 1 imax jmax
    |> List.map (fun x -> x |> List.map (fun (y: Koppen.Zone Option) -> if y.IsNone then "NA\t" else y |> (Option.get >> string >> fun z -> String.concat "" [z; "\t"])))
    |> List.map (fun x -> x |> List.fold (fun acc elem -> String.concat "" [acc; elem]) "" )
    |> fun x -> File.WriteAllLines (@"koppen.out", x)
    
