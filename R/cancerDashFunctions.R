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