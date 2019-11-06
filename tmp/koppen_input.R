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
# lats.name = "lat"
# 
# start.year = 1951
# end.year = 1980
# reference.year = 1850
# calendar = "365"
# koppen.toFS(temperature.file, t.name, precipitation.file, p.name, lats.name, 1951, 1980, 1850, "365")


# file1 = nc_open("/net/c1d/landproj/berkeley/Complete_TAVG_LatLong1.nc")
# t.clim = ncvar_get(file1, "climatology")[c(181:360,1:180),,]
# lats = ncvar_get(file1, "latitude")
# 
# 
# file2 = nc_open("/local/home/landproj/gpcc/precip.mon.total.1x1.v2018.nc")
# p = ncvar_get(file2, "precip", start = c(1,1,709), count = c(-1,-1,360))[,180:1,]
# 
# fdays = mean(apply(as.array(1951:1980), c(1), function(x) { days.in.February(x, "365") }))
# p.clim = apply(p, c(1,2), function(x) { climatology(x, 1951, "365")  })
# p.clim = aperm(p.clim, c(2,3,1))
# 
# koppen.data.toFS(t.clim, p.clim, lats, fdays, "tmp2", "365")

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

koppen.file.toFS = function(
  t.file, t.name, p.file, p.name, latitude.name,
  start.year, end.year, reference.year, path = "tmp", calendar = "Gregorian"){
  
  lats = ncvar_get(t.file, "lat")
  hemisphere = apply(lats, c(1), function(x) { if (x >= 0) 'Northern' else 'Southern' } )
  
  #write.columns = function(data) {
  if(!dir.exists(path)) dir.create(path)
  for(i in 1:360){
    for(j in 1:180){
      t = ncvar_get(t.file, t.name,
                    start = c(i,j,12*(start.year-reference.year - 1) + 1),
                    count = c(1,1,12*(end.year-start.year + 1)))
      if(!is.na(t[1])){
        p = ncvar_get(p.file, p.name,
                      start = c(i,j,12*(start.year-reference.year) + 1),
                      count = c(1,1,12*(end.year-start.year + 1)))
        
        fdays = mean(apply(as.array(start.year:end.year), c(1), function(x) { days.in.February(x, calendar) }))
        
        t.clim = climatology(t, start.year, calendar) - 273.15
        p.clim = climatology(p, start.year, calendar) * 86400 * c(31,fdays,31,30,31,30,31,31,30,31,30,31)
        
        clim = list("daysInFebruary" = fdays, "hemisphere" = hemisphere[j],
                    "temperature" = t.clim, "precipitation" = p.clim)
        write.csv(clim, file = paste0(dir,"/",i,"_",j,".csv"), row.names = FALSE)
      }
    }
  }
  #}
  
}
koppen.file.toFS = cmpfun(koppen.file.toFS)

koppen.data.toFS = function(
  t.clim, p.clim, lats, fdays, path = "tmp", calendar = "Gregorian"){
  
  hemisphere = apply(lats, c(1), function(x) { if (x >= 0) 'Northern' else 'Southern' } )
  
  #write.columns = function(data) {
  if(!dir.exists(path)) dir.create(path)
  for(i in 1:360){
    for(j in 1:180){
      if(!is.na(t.clim[i,j,1]) && !is.na(p.clim[i,j,1])){
        clim = list("daysInFebruary" = fdays, "hemisphere" = hemisphere[j],
                    "temperature" = t.clim[i,j,], "precipitation" = p.clim[i,j,])
        write.csv(clim, file = paste0(path,"/",i,"_",j,".csv"), row.names = FALSE)
      }
    }
  }
  #}
  
}
koppen.data.toFS = cmpfun(koppen.data.toFS)



