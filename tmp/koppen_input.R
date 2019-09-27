library(ncdf4)
library(chron)
library(compiler)
enableJIT(3)

#source("combinators.R")

root = "/local/home/landproj/CMIP6_historical/"
setwd(root)

temperature.file = nc_open("tasLut.nc")
precipitation.file = nc_open("pr.nc")

days.in.February = function(year, calendar = "Julian"){
  if(calendar == "365") 28
  else if(calendar == "Julian") if(year %% 4 == 0) 29 else 28
  else if(calendar == "Gregorian") if(year %% 4 == 0 && (year %% 400 == 0 || year %% 100 != 0)) 29 else 28
  else stop("Invalid calendar option: valid options are 'Julian', 'Gregorian', and '365'")
}

days.in.months = function(year, calendar = "Julian"){
  c(31, days.in.February(year, calendar), 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
}


climatology = function(x, start.year, reference.year){
  nyears = 29
  start = (start.year - reference.year) * 12
  end = (start.year+nyears - reference.year) * 12
    
  asdf = array(NaN, dim = c(12,nyears+1))
  for(i in 1:12){
    for(j in 0:nyears){
      asdf[i,j+1] = x[12*(start.year+j - reference.year) + i]
    }
  }
  apply(asdf, c(1), mean)
  
  # days = array(NaN, dim = c(12,nyears+1))
  # for(j in 0:nyears){
  #   days[,j+1] = days.in.months(start.year+j - reference.year)
  # }
  # apply(asdf, c(1), mean) * apply(days, c(1), mean)
    
  
}

lats = ncvar_get(temperature.file, "lat")
hemisphere = apply(lats, c(1), function(x) { if (x >= 0) 'Northern' else 'Southern' } )

#write.columns = function(data) {
  if(!dir.exists("tmp")) dir.create("tmp")
  for(i in 1:360){
    for(j in 1:180){
      tasLut = ncvar_get(temperature.file, "tasLut", start = c(i,j,1), count = c(1,1,-1))
      if(!is.na(tasLut[1])){
        pr = ncvar_get(precipitation.file, "pr", start = c(i,j,1), count = c(1,1,-1))
        
        t.clim = climatology(tasLut, 1980, 1850) - 273.15
        p.clim = climatology(pr,     1980, 1850) * 86400 * c(31,28,31,30,31,30,31,31,30,31,30,31)
        
        clim = list("daysInFebruary" = 28.0, "hemisphere" = hemisphere[j],
                    "temperature" = t.clim, "precipitation" = p.clim)
        write.csv(clim, file = paste0("tmp/",i,"_",j,".csv"), row.names = FALSE)
      }
    }
  }
#}    

