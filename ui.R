
library(rhandsontable)

shinyUI(fluidPage(
  titlePanel("Sleep Apnea Costs Top-down calculator"),
  fluidRow(
    column(12,
           helpText("Select country to get base values for calculating costs. When OSA table Armeni et al. is selected,  sleep apnea prevalence is taken from Italy (population 15-74 yrs). Selecting OSA table Benjafield et al., sleep apnea prevalences are from their article (population 30-69 yrs). Condition values are country specific from IHME dataset and prevalences which are not available from IHME dataset are base values from Italy. You can change prevalences and costs values to get more specific calculation of the sleep apnea costs. "))
  ),
  fluidRow(
    column(2,
           selectInput("location", label = "Country:", choices = locations, selected = "Finland", multiple = FALSE)),
    column(3, 
           sliderInput("slapnea_prevalence_female", label = "Female Sleep Apnea (AHI≥15) prevalence", min = 0, max = 100, value = 60, step = .01)),
    column(2,
           radioButtons("osa_selected", "OSA based on:", choices = c("Armeni et al.", "Benjafield A. et al.")) ),
  ),
  fluidRow(
    column(2,
           radioButtons("money_index", "Money index correction:", choices = c("No correction", "EuroStat")) ),
    column(3, 
           sliderInput("slapnea_prevalence_male", label = "Male Sleep Apnea (AHI≥15) prevalence", min = 0, max = 100, value = 60, step = .01)),
    column(2, 
           downloadButton("downloadData","Save Data as CSV File"),
           downloadButton("downloadPlot", "Download the plot as png")),
  ),
  fluidRow(
    column(7,
           rHandsontableOutput("hot")
    ), 
    column(5,
           plotOutput("plot")
    )
  )
))