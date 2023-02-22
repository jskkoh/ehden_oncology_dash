# Functions for EDHDEN cancer survival dashboard

# Table for survival plot

survPlotTable <- function(dataSurv,cancerType,age,sex,dataset) {
  tableSurvPlot <- dataSurv %>% 
    filter(Cancer==cancerType & Age==age & Gender==sex & 
             Database==dataset & Method!="Observed") %>%
    select(time,est,lcl,ucl,Method) 
  return(tableSurvPlot)
}

survPlotTableKM <- function(dataKM,cancerType,age,sex,dataset) {
  tableKMPlot <- dataKM %>% 
    filter(Cancer==cancerType & Age==age & Gender==sex & 
             Database==dataset & Method=="Observed") %>%
    select(time,est,lcl,ucl,Method) 
  return(tableKMPlot)
}

hazPlotTable <- function(dataHaz,cancerType,age,sex,dataset) {
  tableHazPlot <- dataHaz %>% 
    filter(Cancer==cancerType & Age==age & Gender==sex & 
             Database==dataset & Method!="Observed") %>%
    select(time,est,lcl,ucl,Method) %>% arrange(Method,time)
  return(tableHazPlot)
}

hazPlotTableObs <- function(dataHazObs,cancerType,age,sex,dataset) {
  tableHazObsPlot <- dataHazObs %>% 
    filter(Cancer==cancerType & Age==age & Gender==sex & 
             Database==dataset & Method=="Observed") %>%
    select(time,est,lcl,ucl,Method) 
  return(tableHazObsPlot)
}

gofTable <- function(dataGOF,cancerType,age,sex,dataset) {
  tableGOF <- dataGOF %>% 
    filter(Cancer==cancerType & Age==age & Gender==sex & Database==dataset) %>%
    select(Method,logLik,AIC,BIC)
  return(tableGOF)
}

riskTable <- function(dataRisk,cancerType,age,sex,dataset) {
  tableRisk <- dataRisk %>% 
    filter(Cancer==cancerType & Age==age & Gender==sex & Database==dataset) %>%
    gather("Time (years)","No. at risk",2:9) %>%
    select(`Time (years)`,`No. at risk`)
  return(tableRisk)
}


survAvgTable <- function(dataSurvAvg,cancerType,age,sex,dataset) {
  tableRisk <- dataSurvAvg %>% 
    filter(Cancer==cancerType & Age==age & Gender==sex & Database==dataset) %>%
  return(tableRisk)
}

