
koppen.file = as.matrix(read.csv("koppen.out", sep = "\t", header = FALSE))

koppen.zones = c("Af", "Am", "Aw", 
                 "BWh", "BWk", "BSh", "BSk",
                 "Csa", "Csb", "Csc",
                 "Cwa", "Cwb", "Cwc",
                 "Cfa", "Cfb", "Cfc",
                 "Dsa", "Dsb", "Dsc", "Dsd",
                 "Dwa", "Dwb", "Dwc", "Dwd",
                 "Dfa", "Dfb", "Dfc", "Dfc",
                 "ET", "EF")
koppen.colors = c("#0000FF", "#0078FF", "#46AAFA", 
                  "#FF0000", "#FF9696", "#F5A500", "#FFDC64",
                  "#FFFF00", "#C6C700", "#969600",
                  "#96FF96", "#63C763", "#329632",
                  "#C8FF50", "#66FF33", "#32C700",
                  "#FF00FE", "#C600C7", "#963295", "#966495",
                  "#ABB1FF", "#5A77DB", "#4C51B5", "#320087",
                  "#00FFFF", "#38C7FF", "#007E7D", "#00455E",
                  "#B2B2B2", "#686868")

koppen.masks = array(NA, dim = c(length(koppen.zones), dim(koppen.file)))
for(i in 1:length(koppen.zones)){
  koppen.masks[i,,] = apply(koppen.file, c(1,2), function(x) {x == koppen.zones[i]})
}

koppen.map = apply(koppen.file, c(1,2), function(x) {match(x, koppen.zones)})
filled.contour(koppen.map, col = koppen.colors, nlevels = length(koppen.colors))
