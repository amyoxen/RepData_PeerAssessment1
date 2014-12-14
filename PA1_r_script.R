####This r script is to test the R input of the PA1 report####

actdata$date<-as.POSIXct(strptime(actdata$date, format="%Y-%m-%d"))

with(actdata,plot(steps,))