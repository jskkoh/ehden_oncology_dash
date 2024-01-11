# Functions for EDHDEN cancer survival dashboard

# Table for survival plot
survPlotTable <- function(dataSurv,cancerType,age,sex,dataset) {
  tableSurvPlot <- dataSurv %>% 
    filter(Cancer==cancerType & Age==age & Sex==sex & 
             Database==dataset & Method!="Observed") %>%
    select(time,est,lcl,ucl,Method) 
  return(tableSurvPlot)
}

survPlotTableKM <- function(dataKM,cancerType,age,sex,dataset) {
  tableKMPlot <- dataKM %>% 
    filter(Cancer==cancerType & Age==age & Sex==sex & 
             Database==dataset & Method=="Observed") %>%
    select(time,est,lcl,ucl,Method) 
  return(tableKMPlot)
}

hazPlotTable <- function(dataHaz,cancerType,age,sex,dataset) {
  tableHazPlot <- dataHaz %>% 
    filter(Cancer==cancerType & Age==age & Sex==sex & 
             Database==dataset & Method!="Observed") %>%
    select(time,est,lcl,ucl,Method) %>% arrange(Method,time)
  return(tableHazPlot)
}

hazPlotTableObs <- function(dataHazObs,cancerType,age,sex,dataset) {
  tableHazObsPlot <- dataHazObs %>% 
    filter(Cancer==cancerType & Age==age & Sex==sex & 
             Database==dataset & Method=="Observed") %>%
    select(time,est,lcl,ucl,Method) 
  return(tableHazObsPlot)
}

gofTable <- function(dataGOF,cancerType,age,sex,dataset) {
  tableGOF <- dataGOF %>% 
    filter(Cancer==cancerType & Age==age & Sex==sex & Database==dataset) %>%
    select(Method,logLik,AIC,BIC)
  return(tableGOF)
}

riskTable <- function(dataRisk,cancerType,age,sex,dataset) {
  tableRisk <- dataRisk %>% 
    filter(Cancer==cancerType & Age==age & Sex==sex & Database==dataset) 
  tableRisk1 <- tableRisk %>% filter(details=="n.risk") %>%
    gather("Time (years)","No. at risk",3:24) %>% 
    mutate(`Time (years)`=substr(`Time (years)`,2,nchar(`Time (years)`))) %>%
    select(`Time (years)`,`No. at risk`)
  tableRisk2 <- tableRisk %>% filter(details=="n.event") %>%
    gather("Time (years)","No. events",3:24) %>% 
    mutate(`Time (years)`=substr(`Time (years)`,2,nchar(`Time (years)`))) %>%
    select(`No. events`)
  tableRisk <- cbind(tableRisk1,tableRisk2)
  return(tableRisk)
}


survAvgTable <- function(dataSurvAvg,cancerType,age,sex,dataset) {
  tableRisk <- dataSurvAvg %>% 
    filter(Cancer==cancerType & Age==age & Sex==sex & Database==dataset) %>%
  return(tableRisk)
}

