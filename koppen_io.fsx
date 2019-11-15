
#I @"/home/c1d/.nuget/packages/fsharp.data/3.3.2/lib/net45/"
#r @"FSharp.Data.dll"
#load @"koppen.fs"
open Koppen
open System.IO
open FSharp.Data


type KoppenFile = CsvProvider<"/local/home/landproj/CMIP6_historical/tmp2/100_135.csv",
                              HasHeaders = true,
                              Schema = "float, string, float, float">
let file = KoppenFile.Load("/local/home/landproj/CMIP6_historical/tmp2/100_135.csv")

let imax = 360
let jmax = 180

let readKoppen imax jmax (timer: System.Diagnostics.Stopwatch) =

    let rec readKoppenCol i imax jmax (timer: System.Diagnostics.Stopwatch) =
        let rec readKoppenRow i j jmax (timer: System.Diagnostics.Stopwatch) =
            let fname = String.concat "" ["/local/home/landproj/CMIP6_historical/tmp2/"; string i; "_"; string j; ".csv"]
            if not(File.Exists fname) then
                if j = jmax then [None] else None :: (readKoppenRow i (j+1) jmax timer)
            else
                let file = KoppenFile.Load(fname)

                let daysInFebruary = (file.Rows |> Seq.head).DaysInFebruary * 1.0<day>
                let hemisphere = (file.Rows |> Seq.head).Hemisphere |> function | "Northern" -> Koppen.Northern | "Southern" -> Koppen.Southern | _ -> failwith "fail"
                let temperature =   file.Rows |> Seq.map ((fun x -> x.Temperature)   >> float >> (*) 1.0<C>) |> Seq.toArray
                let precipitation = file.Rows |> Seq.map ((fun x -> x.Precipitation) >> float >> (*) 1.0<mm>) |> Seq.toArray

                let clim = Koppen.Climate (daysInFebruary, hemisphere, temperature, precipitation)
                timer.Start()
                let zone = Koppen.Zones clim
                timer.Stop()
                if j = jmax then [Some zone] else Some zone :: (readKoppenRow i (j+1) jmax timer)
        let row = readKoppenRow i 1 jmax timer
        if i = imax then [row] else row :: (readKoppenCol (i+1) imax jmax timer)

    readKoppenCol 1 imax jmax timer
    |> List.map (fun x -> x |> List.map (fun (y: Koppen.Zone Option) -> if y.IsNone then "NA\t" else y |> (Option.get >> string >> fun z -> String.concat "" [z; "\t"])))
    |> List.map (fun x -> x |> List.fold (fun acc elem -> String.concat "" [acc; elem]) "" )
    |> fun x -> File.WriteAllLines (@"/local/home/landproj/CMIP6_historical/koppen_empirical.out", x)


let koppenTimer = System.Diagnostics.Stopwatch.StartNew()
readKoppen imax jmax koppenTimer
printfn "%i ms\n" koppenTimer.ElapsedMilliseconds
