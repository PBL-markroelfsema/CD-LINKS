calcBudget_historical = function(data,var,new_var){
  if(!(var %in% data$variable)){
    stop("Error: The dataframe does not contain the variable provided!")
  }
  
  #dat=data[variable %in% var]
  dat=data
  ######budget 1850-1989
  # Interpolate to get values for each year
  yy=seq(1850,2015)
  dt = dat[,list(approx(x=year,y=value,xout=yy)$y,approx(x=year,y=value,xout=yy)$x),by=c('variable','scenario','region','unit','source','statistic')]
  setnames(dt,"V1","value")
  setnames(dt,"V2","period")
  
  # Sum to get budget
  budget1=dt[year %in% c(1850:1989),sum(value/1000,na.rm=TRUE),by=c('variable','scenario','region','unit','source','statistic')]
  setnames(budget1,"V1","value")
  
  #correctly specify remaining dimensions
  budget1$variable = new_var
  budget1$unit = 'Gt CO2'
  budget$period='1990'
  
  #######budget 1990-2015
  # Interpolate to get values for each year
  yy=seq(2005,2100)
  dt = dat[,list(approx(x=year,y=value,xout=yy)$y,approx(x=year,y=value,xout=yy)$x),by=c('scenario','Category','Baseline','model','region','Scope','unit','variable')]
  dt=dat
  setnames(dt,"V1","value")
  setnames(dt,"V2","period")
  
  # Sum to get budget
  budget2=dt[year %in% c(1990:2015),sum(value/1000,na.rm=TRUE),by=c('variable','scenario','region','unit','source','statistic')]
  setnames(budget2,"V1","value")
  
  #correctly specify remaining dimensions
  budget2$variable = new_var
  budget2$unit = 'Gt CO2'
  budget2$period='2015'
  
  #merge data
  setcolorder(budget1,c('scenario','Category','Baseline','model','region','period','Scope','value','unit','variable'))
  setcolorder(budget2,c('scenario','Category','Baseline','model','region','period','Scope','value','unit','variable'))
  data <- rbind(data,budget1,budget2, fill=TRUE)
  
  # convert to data.table / data.frame and return
  return(as.data.table(as.data.frame(data)))
}  