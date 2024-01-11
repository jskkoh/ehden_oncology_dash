
input <- list(
  cancerType="Breast",
  age="All",
  sex="Both",
  dataset="CPRD_GOLD"
)

tableSurvPlot = survPlotTable(
  surv_df_all,
  input$cancerType,
  input$age,
  input$sex,
  input$dataset
)

tableSurvPlotKM = survPlotTableKM(
  surv_df_all,
  input$cancerType,
  input$age,
  input$sex,
  input$dataset
)


tableHazPlot = hazPlotTable(
  rateHaz_df_all,
  input$cancerType,
  input$age,
  input$sex,
  input$dataset
)

tableHazObsPlot = hazPlotTableObs(
  rateHaz_df_all,
  input$cancerType,
  input$age,
  input$sex,
  input$dataset
)

tableGOF = gofTable(
  gof_df_all,
  input$cancerType,
  input$age,
  input$sex,
  input$dataset
)

tableRisk = riskTable(
  riskTable_df_all,
  input$cancerType,
  input$age,
  input$sex,
  input$dataset
)



tableSurvAvg = survAvgTable(
  survAvg_df_all,
  input$cancerType,
  input$age,
  input$sex,
  input$dataset
)

meanSurv =  pull(tableSurvAvg %>% select(meanSurv))
medianSurv =  pull(tableSurvAvg %>% select(medianSurv))
