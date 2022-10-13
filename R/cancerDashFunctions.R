# Functions for EDHDEN cancer survival dashboard

# Table for survival plot

survPlotTable <- function(dataSurv,cancerType,age,sex) {
  tableSurvPlot <- dataSurv %>% 
    filter(Cancer==cancerType & Age==age & Gender==sex) %>%
    select(time,est,lcl,ucl,Method) 
  return(tableSurvPlot)
}

survPlotTableKM <- function(dataKM,cancerType,age,sex) {
  tableKMPlot <- dataKM %>% 
    filter(Cancer==cancerType & Age==age & Gender==sex) %>%
    select(time,est,lcl,ucl,Method) 
  return(tableKMPlot)
}

hazPlotTable <- function(dataHaz,cancerType,age,sex) {
  tableHazPlot <- dataHaz %>% 
    filter(Cancer==cancerType & Age==age & Gender==sex) %>%
    select(time,est,lcl,ucl,Method) %>% arrange(Method,time)
  return(tableHazPlot)
}

hazPlotTableObs <- function(dataHazObs,cancerType,age,sex) {
  tableHazObsPlot <- dataHazObs %>% 
    filter(Cancer==cancerType & Age==age & Gender==sex) %>%
    select(time,est,lcl,ucl,Method) 
  return(tableHazObsPlot)
}

gofTable <- function(dataGOF,cancerType,age,sex) {
  tableGOF <- dataGOF %>% 
    filter(Cancer==cancerType & Age==age & Gender==sex) %>%
    select(-Cancer,-Age,-Gender)
  colnames(tableGOF) <- c("N","Events","Censored","Time at risk","df",
                          "logLik","AIC","BIC","Method")
  tableGOF <- tableGOF[,c(9,1,2,3,4,5,6,7,8)]
  return(tableGOF)
}