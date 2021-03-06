
#load "koppen.fs"
open Koppen

let myAf =  Koppen.Climate (28.25<day>, Koppen.Northern,
              [| 20.0<C>;   21.0<C>;   23.0<C>;   21.0<C>;   23.0<C>;   26.0<C>;   20.0<C>;   21.0<C>;   19.0<C>;   22.0<C>;   23.0<C>;   21.0<C>|],
              [|72.4<mm>; 83.32<mm>; 85.38<mm>; 79.53<mm>; 81.79<mm>; 82.01<mm>; 82.11<mm>; 81.94<mm>; 79.88<mm>; 75.89<mm>; 74.56<mm>; 72.43<mm>|])
let myAm = Koppen.Climate (28.25<day>, Koppen.Northern,
              [| 20.0<C>;   21.0<C>;   23.0<C>;    21.0<C>;    23.0<C>;    26.0<C>;    20.0<C>;    21.0<C> ;   19.0<C>;    22.0<C>;   23.0<C>;   21.0<C>|],
              [|55.4<mm>; 83.32<mm>; 95.38<mm>; 121.53<mm>; 161.79<mm>; 162.01<mm>; 162.11<mm>; 161.94<mm>; 129.88<mm>; 105.89<mm>; 84.56<mm>; 62.43<mm>|])
let myAw = Koppen.Climate (28.25<day>, Koppen.Northern,
              [|20.0<C>;   21.0<C>;   23.0<C>;   21.0<C>;    23.0<C>;    26.0<C>;    20.0<C>;    21.0<C>;   19.0<C>;   22.0<C>;  23.0<C>;  21.0<C>|],
              [|2.4<mm>; 13.32<mm>; 25.38<mm>; 49.53<mm>; 121.79<mm>; 182.01<mm>; 193.11<mm>; 181.94<mm>; 49.88<mm>; 35.89<mm>; 4.56<mm>; 2.43<mm>|])


let myBSh = Koppen.Climate (28.25<day>, Koppen.Northern,
              [| -2.0<C>;    1.0<C>;    3.0<C>;   10.0<C>;   13.0<C>;   16.0<C>;   20.0<C>;   21.0<C>;   19.0<C>;   12.0<C>;    3.0<C>;    1.0<C>|],
              [|12.4<mm>; 13.32<mm>; 15.38<mm>; 19.53<mm>; 21.79<mm>; 22.01<mm>; 22.11<mm>; 21.94<mm>; 19.88<mm>; 15.89<mm>; 14.56<mm>; 12.43<mm>|])
let myBSk = Koppen.Climate (28.25<day>, Koppen.Northern,
              [| -4.0<C>;    1.0<C>;    3.0<C>;   10.0<C>;   13.0<C>;   16.0<C>;   20.0<C>;   21.0<C>;   19.0<C>;   12.0<C>;    3.0<C>;    1.0<C>|],
              [|12.4<mm>; 13.32<mm>; 15.38<mm>; 19.53<mm>; 21.79<mm>; 22.01<mm>; 22.11<mm>; 21.94<mm>; 19.88<mm>; 15.89<mm>; 14.56<mm>; 12.43<mm>|])
let myBWh = Koppen.Climate (28.25<day>, Koppen.Northern,
              [|-2.0<C>;   1.0<C>;   3.0<C>;  10.0<C>;   13.0<C>;   16.0<C>;   20.0<C>;  21.0<C>;  19.0<C>;  12.0<C>;   3.0<C>;   1.0<C>|],
              [|2.4<mm>; 3.32<mm>; 5.38<mm>; 9.53<mm>; 11.79<mm>; 12.01<mm>; 12.11<mm>; 9.94<mm>; 5.88<mm>; 1.89<mm>; 1.56<mm>; 1.43<mm>|])
let myBWk = Koppen.Climate (28.25<day>, Koppen.Northern,
              [| -4.0<C>;  1.0<C>;   3.0<C>;  10.0<C>;   13.0<C>;   16.0<C>;   20.0<C>;  21.0<C>;  19.0<C>;  12.0<C>;   3.0<C>;   1.0<C>|],
              [|2.4<mm>; 3.32<mm>; 5.38<mm>; 9.53<mm>; 11.79<mm>; 12.01<mm>; 12.11<mm>; 9.94<mm>; 5.88<mm>; 1.89<mm>; 1.56<mm>; 1.43<mm>|])


let myCfa = Koppen.Climate (28.25<day>, Koppen.Northern,
              [|  -2.0<C>;     1.0<C>;    3.0<C>;   10.0<C>;   13.0<C>;  16.0<C>;  20.0<C>;  22.1<C>;   19.0<C>;   12.0<C>;    3.0<C>;    1.0<C>|],
              [|50.4<mm>; 44.32<mm>; 45.38<mm>; 51.53<mm>; 62.79<mm>; 78.01<mm>; 79.11<mm>; 78.94<mm>; 69.88<mm>; 61.89<mm>; 54.56<mm>; 53.43<mm>|])
let myCfb = Koppen.Climate (28.25<day>, Koppen.Northern,
              [|  -2.0<C>;     1.0<C>;    3.0<C>;   10.0<C>;   13.0<C>;  16.0<C>;  20.0<C>;  20.1<C>;   19.0<C>;   12.0<C>;    3.0<C>;    1.0<C>|],
              [|50.4<mm>; 44.32<mm>; 45.38<mm>; 51.53<mm>; 62.79<mm>; 78.01<mm>; 79.11<mm>; 78.94<mm>; 69.88<mm>; 61.89<mm>; 54.56<mm>; 53.43<mm>|])
let myCfc = Koppen.Climate (28.25<day>, Koppen.Northern,
              [|  -2.0<C>;     1.0<C>;    3.0<C>;    5.0<C>;    7.3<C>;   9.0<C>;  12.0<C>;  11.0<C>;    9.5<C>;    6.0<C>;    2.0<C>;    1.0<C>|],
              [|50.4<mm>; 44.32<mm>; 45.38<mm>; 51.53<mm>; 62.79<mm>; 78.01<mm>; 79.11<mm>; 78.94<mm>; 69.88<mm>; 61.89<mm>; 54.56<mm>; 53.43<mm>|])

let myCwa = Koppen.Climate (28.25<day>, Koppen.Northern,
              [| -2.0<C>;   1.0<C>;    3.0<C>;   10.0<C>;   13.0<C>;   16.0<C>;   20.0<C>;    22.1<C>;   19.0<C>;   12.0<C>;    3.0<C>;   1.0<C>|],
              [|9.11<mm>; 8.94<mm>; 19.88<mm>; 25.89<mm>; 34.56<mm>; 79.43<mm>; 120.4<mm>; 110.32<mm>; 45.38<mm>; 19.53<mm>; 10.79<mm>; 8.01<mm>|])
let myCwb = Koppen.Climate (28.25<day>, Koppen.Northern,
              [| -2.0<C>;   1.0<C>;    3.0<C>;   10.0<C>;   13.0<C>;   16.0<C>;   20.0<C>;    20.1<C>;   19.0<C>;   12.0<C>;    3.0<C>;    1.0<C>|],
              [|9.11<mm>; 8.94<mm>; 19.88<mm>; 25.89<mm>; 34.56<mm>; 79.43<mm>; 120.4<mm>; 110.32<mm>; 45.38<mm>; 19.53<mm>; 10.79<mm>; 8.01<mm>|])
let myCwc = Koppen.Climate (28.25<day>, Koppen.Northern,
              [| -2.0<C>;   1.0<C>;    3.0<C>;    5.0<C>;    7.3<C>;    9.0<C>;   12.0<C>;    11.0<C>;    9.5<C>;    6.0<C>;    2.0<C>;   1.0<C>|],
              [|9.11<mm>; 8.94<mm>; 19.88<mm>; 25.89<mm>; 34.56<mm>; 79.43<mm>; 120.4<mm>; 110.32<mm>; 45.38<mm>; 19.53<mm>; 10.79<mm>; 8.01<mm>|])

let myCsa = Koppen.Climate (28.25<day>, Koppen.Northern,
              [|  -2.0<C>;     1.0<C>;    3.0<C>;   10.0<C>;   13.0<C>;  16.0<C>;  20.0<C>;  22.1<C>;   19.0<C>;   12.0<C>;    3.0<C>;    1.0<C>|],
              [|120.4<mm>; 110.32<mm>; 45.38<mm>; 19.53<mm>; 10.79<mm>; 8.01<mm>; 9.11<mm>; 8.94<mm>; 19.88<mm>; 25.89<mm>; 34.56<mm>; 79.43<mm>|])
let myCsb = Koppen.Climate (28.25<day>, Koppen.Northern,
              [|  -2.0<C>;     1.0<C>;    3.0<C>;   10.0<C>;   13.0<C>;  16.0<C>;  20.0<C>;  20.0<C>;   19.0<C>;   12.0<C>;    3.0<C>;    1.0<C>|],
              [|120.4<mm>; 110.32<mm>; 45.38<mm>; 19.53<mm>; 10.79<mm>; 8.01<mm>; 9.11<mm>; 8.94<mm>; 19.88<mm>; 25.89<mm>; 34.56<mm>; 79.43<mm>|])
let myCsc = Koppen.Climate (28.25<day>, Koppen.Northern,
              [|  -2.0<C>;     1.0<C>;    3.0<C>;    5.0<C>;    7.3<C>;   9.0<C>;  12.0<C>;  11.0<C>;    9.5<C>;    6.0<C>;    2.0<C>;    1.0<C>|],
              [|120.4<mm>; 110.32<mm>; 45.38<mm>; 19.53<mm>; 10.79<mm>; 8.01<mm>; 9.11<mm>; 8.94<mm>; 19.88<mm>; 25.89<mm>; 34.56<mm>; 79.43<mm>|])


let myDfa = Koppen.Climate (28.25<day>, Koppen.Northern,
              [|  -4.0<C>;     1.0<C>;    3.0<C>;   10.0<C>;   13.0<C>;  16.0<C>;  20.0<C>;  22.1<C>;   19.0<C>;   12.0<C>;    3.0<C>;    1.0<C>|],
              [|50.4<mm>; 44.32<mm>; 45.38<mm>; 51.53<mm>; 62.79<mm>; 78.01<mm>; 79.11<mm>; 78.94<mm>; 69.88<mm>; 61.89<mm>; 54.56<mm>; 53.43<mm>|])
let myDfb = Koppen.Climate (28.25<day>, Koppen.Northern,
              [|  -4.0<C>;     1.0<C>;    3.0<C>;   10.0<C>;   13.0<C>;  16.0<C>;  20.0<C>;  20.1<C>;   19.0<C>;   12.0<C>;    3.0<C>;    1.0<C>|],
              [|50.4<mm>; 44.32<mm>; 45.38<mm>; 51.53<mm>; 62.79<mm>; 78.01<mm>; 79.11<mm>; 78.94<mm>; 69.88<mm>; 61.89<mm>; 54.56<mm>; 53.43<mm>|])
let myDfc = Koppen.Climate (28.25<day>, Koppen.Northern,
              [|  -4.0<C>;     1.0<C>;    3.0<C>;    5.0<C>;    7.3<C>;   9.0<C>;  12.0<C>;  11.0<C>;    9.5<C>;    6.0<C>;    2.0<C>;    1.0<C>|],
              [|50.4<mm>; 44.32<mm>; 45.38<mm>; 51.53<mm>; 62.79<mm>; 78.01<mm>; 79.11<mm>; 78.94<mm>; 69.88<mm>; 61.89<mm>; 54.56<mm>; 53.43<mm>|])
let myDfd = Koppen.Climate (28.25<day>, Koppen.Northern,
              [|-40.0<C>;    -35.0<C>;  -33.0<C>;  -10.0<C>;   -8.0<C>;  -5.0<C>;   2.0<C>;   11.0<C>;   -3.0<C>;  -12.0<C>;  -20.0<C>;  -29.0<C>|],
              [|50.4<mm>; 44.32<mm>; 45.38<mm>; 51.53<mm>; 62.79<mm>; 78.01<mm>; 79.11<mm>; 78.94<mm>; 69.88<mm>; 61.89<mm>; 54.56<mm>; 53.43<mm>|])

let myDwa = Koppen.Climate (28.25<day>, Koppen.Northern,
              [| -4.0<C>;   1.0<C>;    3.0<C>;   10.0<C>;   13.0<C>;   16.0<C>;   20.0<C>;    22.1<C>;   19.0<C>;   12.0<C>;    3.0<C>;   1.0<C>|],
              [|9.11<mm>; 8.94<mm>; 19.88<mm>; 25.89<mm>; 34.56<mm>; 79.43<mm>; 120.4<mm>; 110.32<mm>; 45.38<mm>; 19.53<mm>; 10.79<mm>; 8.01<mm>|])
let myDwb = Koppen.Climate (28.25<day>, Koppen.Northern,
              [| -4.0<C>;   1.0<C>;    3.0<C>;   10.0<C>;   13.0<C>;   16.0<C>;   20.0<C>;    20.1<C>;   19.0<C>;   12.0<C>;    3.0<C>;    1.0<C>|],
              [|9.11<mm>; 8.94<mm>; 19.88<mm>; 25.89<mm>; 34.56<mm>; 79.43<mm>; 120.4<mm>; 110.32<mm>; 45.38<mm>; 19.53<mm>; 10.79<mm>; 8.01<mm>|])
let myDwc = Koppen.Climate (28.25<day>, Koppen.Northern,
              [| -4.0<C>;   1.0<C>;    3.0<C>;    5.0<C>;    7.3<C>;    9.0<C>;   12.0<C>;    11.0<C>;    9.5<C>;    6.0<C>;    2.0<C>;   1.0<C>|],
              [|9.11<mm>; 8.94<mm>; 19.88<mm>; 25.89<mm>; 34.56<mm>; 79.43<mm>; 120.4<mm>; 110.32<mm>; 45.38<mm>; 19.53<mm>; 10.79<mm>; 8.01<mm>|])
let myDwd = Koppen.Climate (28.25<day>, Koppen.Northern,
              [|-40.0<C>;    -35.0<C>;  -33.0<C>;  -10.0<C>;   -8.0<C>;  -5.0<C>;   2.0<C>;   11.0<C>;   -3.0<C>;  -12.0<C>;  -20.0<C>;  -29.0<C>|],
              [|9.11<mm>; 8.94<mm>; 19.88<mm>; 25.89<mm>; 34.56<mm>; 79.43<mm>; 120.4<mm>; 110.32<mm>; 45.38<mm>; 19.53<mm>; 10.79<mm>; 8.01<mm>|])

let myDsa = Koppen.Climate (28.25<day>, Koppen.Northern,
              [|  -4.0<C>;     1.0<C>;    3.0<C>;   10.0<C>;   13.0<C>;  16.0<C>;  20.0<C>;  22.1<C>;   19.0<C>;   12.0<C>;    3.0<C>;    1.0<C>|],
              [|120.4<mm>; 110.32<mm>; 45.38<mm>; 19.53<mm>; 10.79<mm>; 8.01<mm>; 9.11<mm>; 8.94<mm>; 19.88<mm>; 25.89<mm>; 34.56<mm>; 79.43<mm>|])
let myDsb = Koppen.Climate (28.25<day>, Koppen.Northern,
              [|  -4.0<C>;     1.0<C>;    3.0<C>;   10.0<C>;   13.0<C>;  16.0<C>;  20.0<C>;  20.0<C>;   19.0<C>;   12.0<C>;    3.0<C>;    1.0<C>|],
              [|120.4<mm>; 110.32<mm>; 45.38<mm>; 19.53<mm>; 10.79<mm>; 8.01<mm>; 9.11<mm>; 8.94<mm>; 19.88<mm>; 25.89<mm>; 34.56<mm>; 79.43<mm>|])
let myDsc = Koppen.Climate (28.25<day>, Koppen.Northern,
              [|  -4.0<C>;     1.0<C>;    3.0<C>;    5.0<C>;    7.3<C>;   9.0<C>;  12.0<C>;  11.0<C>;    9.5<C>;    6.0<C>;    2.0<C>;    1.0<C>|],
              [|120.4<mm>; 110.32<mm>; 45.38<mm>; 19.53<mm>; 10.79<mm>; 8.01<mm>; 9.11<mm>; 8.94<mm>; 19.88<mm>; 25.89<mm>; 34.56<mm>; 79.43<mm>|])
let myDsd = Koppen.Climate (28.25<day>, Koppen.Northern,
              [|-40.0<C>;    -35.0<C>;  -33.0<C>;  -10.0<C>;   -8.0<C>;  -5.0<C>;   2.0<C>;   11.0<C>;   -3.0<C>;  -12.0<C>;  -20.0<C>;  -29.0<C>|],
              [|120.4<mm>; 110.32<mm>; 45.38<mm>; 19.53<mm>; 10.79<mm>; 8.01<mm>; 9.11<mm>; 8.94<mm>; 19.88<mm>; 25.89<mm>; 34.56<mm>; 79.43<mm>|])


let myET = Koppen.Climate (28.25<day>, Koppen.Northern,
              [|-40.0<C>;    -35.0<C>;  -33.0<C>;  -10.0<C>;   -8.0<C>;  -5.0<C>;   2.0<C>;   1.0<C>;   -3.0<C>;  -12.0<C>;  -20.0<C>;  -29.0<C>|],
              [|120.4<mm>; 110.32<mm>; 45.38<mm>; 19.53<mm>; 10.79<mm>; 8.01<mm>; 9.11<mm>; 8.94<mm>; 19.88<mm>; 25.89<mm>; 34.56<mm>; 79.43<mm>|])
let myEF = Koppen.Climate (28.25<day>, Koppen.Northern,
              [|-40.0<C>;    -35.0<C>;  -33.0<C>;  -10.0<C>;   -8.0<C>;  -5.0<C>;  -2.0<C>;  -1.0<C>;   -3.0<C>;  -12.0<C>;  -20.0<C>;  -29.0<C>|],
              [|120.4<mm>; 110.32<mm>; 45.38<mm>; 19.53<mm>; 10.79<mm>; 8.01<mm>; 9.11<mm>; 8.94<mm>; 19.88<mm>; 25.89<mm>; 34.56<mm>; 79.43<mm>|])

let myClimates = [myAf; myAm; myAw;
                  myBWh; myBWk; myBSh; myBSk;
                  myCsa; myCsb; myCsc;
                  myCwa; myCwb; myCwc;
                  myCfa; myCfb; myCfc;
                  myDsa; myDsb; myDsc; myDsd;
                  myDwa; myDwb; myDwc; myDwd;
                  myDfa; myDfb; myDfc; myDfd;
                  myET; myEF]

myClimates |> List.map (Koppen.Zones >> string >> printf "%s\n") |> ignore
