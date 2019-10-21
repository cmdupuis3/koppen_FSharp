library(ncdf4)
library(compiler)
enableJIT(3)

#source("combinators.R")

root = "/local/home/landproj/CMIP6_historical/"
setwd(root)

# temperature.file = nc_open("tasLut.nc")
# precipitation.file = nc_open("pr.nc")
# t.name = "tasLut"
# p.name = "pr"
# 
# start.year = 1951
# end.year = 1980
# reference.year = 1850
# calendar = "365"
# koppen.toFS(temperature.file, t.name, precipitation.file, p.name, 1951, 1980, 1850, "365")


days.in.February = function(year, calendar = "Gregorian"){
  if(calendar == "365") 28
  else if(calendar == "Julian") if(year %% 4 == 0) 29 else 28
  else if(calendar == "Gregorian") if(year %% 4 == 0 && (year %% 400 == 0 || year %% 100 != 0)) 29 else 28
  else stop("Invalid calendar option: valid options are 'Julian', 'Gregorian', and '365'")
}

days.in.months = function(year, calendar = "Gregorian"){
  c(31, days.in.February(year, calendar), 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
}

climatology = function(x, start.year, calendar = "Gregorian"){
  
  if(length(x) %% 12 != 0)
    stop("Only complete years allowed")
  
  nyears = length(x) / 12
  
  asdf = array(NaN, dim = c(12,nyears+1))
  for(i in 1:12){
    for(j in 0:nyears){
      asdf[i,j+1] = x[12*j + i]
    }
  }
  apply(asdf, c(1), mean)
  
  days = array(NaN, dim = c(12,nyears+1))
  for(j in 0:nyears){
    days[,j+1] = days.in.months(start.year+j, calendar)
  }
  
  clim = rep(NaN, 12)
  for(i in 1:12){
    clim[i] = weighted.mean(asdf[i,], days[i,], na.rm = TRUE)
  }
  clim
  
}

koppen.toFS = function(t.file, t.name, p.file, p.name, start.year, end.year, reference.year, calendar = "Gregorian"){

  lats = ncvar_get(temperature.file, "lat")
  hemisphere = apply(lats, c(1), function(x) { if (x >= 0) 'Northern' else 'Southern' } )
  
  #write.columns = function(data) {
    if(!dir.exists("tmp")) dir.create("tmp")
    for(i in 1:360){
      for(j in 1:180){
        t = ncvar_get(t.file, t.name,
                      start = c(i,j,12*(start.year-reference.year - 1) + 1),
                      count = c(1,1,12*(end.year-start.year + 1)))
        if(!is.na(tasLut[1])){
          p = ncvar_get(p.file, p.name,
                        start = c(i,j,12*(start.year-reference.year) + 1),
                        count = c(1,1,12*(end.year-start.year + 1)))
          
          fdays = mean(apply(as.array(start.year:end.year), c(1), function(x) { days.in.February(x, calendar) }))
          
          t.clim = climatology(t, start.year, calendar) - 273.15
          p.clim = climatology(p, start.year, calendar) * 86400 * c(31,fdays,31,30,31,30,31,31,30,31,30,31)
  
          clim = list("daysInFebruary" = fdays, "hemisphere" = hemisphere[j],
                      "temperature" = t.clim, "precipitation" = p.clim)
          write.csv(clim, file = paste0("tmp/",i,"_",j,".csv"), row.names = FALSE)
        }
      }
    }
  #}

}


