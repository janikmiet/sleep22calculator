

shinyUI(fluidPage(
  titlePanel("Sleep Apnea Costs Top-down Calculator"),
  fluidRow(
    column(12,
           helpText("Select a country to get base values for calculating the costs. Sleep Apnea prevalences are from Benjafield et al. article (population 30-69 yrs). You can input new gender specific sleep apnea prevalences with the corresponding slider input. Condition prevalence values are country specific from IHME dataset. Condition prevalences which are not available from IHME dataset are base values from Italy (Armeni et al.) as well as condition annual costs. You can modify prevalences and costs values to get more specific calculation of the sleep apnea costs. Calculation does not take in account of total healthcare service costs if sleep apnea prevalence rises."))
  ),
  fluidRow(
    column(2,
           selectInput("location", label = "Country:", choices = locations, selected = "Finland", multiple = FALSE)),
    column(3, 
           sliderInput("slapnea_prevalence_female", label = "Female Sleep Apnea (AHI≥15) prevalence", min = 0, max = 100, value = 60, step = .01)),
    column(2,
           radioButtons("osa_selected", "Prevalences (OSA & Conditions):", choices = c("Country specific estimations", "Fixed, based on Italy")) ),
  ),
  fluidRow(
    column(2,
           radioButtons("money_index", "Money index correction:", choices = c("No correction", "EuroStat '19")) ),
    column(3, 
           sliderInput("slapnea_prevalence_male", label = "Male Sleep Apnea (AHI≥15) prevalence", min = 0, max = 100, value = 60, step = .01)),
    column(2, 
           downloadButton("downloadData","Download calculation data as CSV"),
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