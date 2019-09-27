
koppen.file = as.matrix(read.csv("koppen.out", sep = "\t", header = FALSE))

koppen.zones = sort(unique(as.vector(koppen.file)))
koppen.masks = array(NA, dim = c(length(koppen.zones), dim(koppen.file)))
for(i in 1:length(koppen.zones)){
  koppen.masks[i,,] = apply(koppen.file, c(1,2), function(x) {x == koppen.zones[i]})
}

koppen.map = apply(koppen.file, c(1,2), function(x) {match(x, koppen.zones)})
filled.contour(koppen.map)
