
navbarPage(
  
  ## TESTING LOGO ON THE MENU
  title = div(
    div(
      id = "img-id",
      # img(src = "img/alllogos.png")
    ),
    "Sleep Apnea Cost Calculator"
  ),
  # theme = "bootstrap.css",
  # title = "Sleep Apnea Cost Calculator", 
  # id="nav",
  tags$head(
    # Include our custom CSS
    includeCSS("styles.css"),
  ),
  
  tabPanel("Calculator",
           fluidPage(
             fluidRow(
               column(12,
                      helpText("Select a country to get base values for calculating the costs. Sleep Apnea prevalences are from Benjafield et al. article. You can set new gender specific sleep apnea prevalences with the slider inputs. Condition prevalence values are country specific from IHME dataset. Condition prevalences which are not available from IHME dataset are base values from Italy (Armeni et al.) as well as condition annual costs. You can modify prevalences and costs values to get more specific calculation of the sleep apnea costs. Calculation does not take in account of total healthcare service costs if sleep apnea prevalence rises."))
             ),
             fluidRow(
               column(6,
                      fixedRow(
                        column(4,
                               selectInput("location", label = "Country:", choices = locations, selected = "Finland", multiple = FALSE)),
                        column(4, 
                               sliderInput("slapnea_prevalence_female", label = "Female Sleep Apnea (AHI≥15) prevalence", min = 1, max = 85, value = 60, step = .01)),
                        column(4,
                               radioButtons("osa_selected", "Prevalences (OSA & Conditions):", choices = c("Country specific estimations", "Fixed, based on Italy")) 
                        )
                      ), # saadot 1
                      fixedRow(
                        column(4,
                               radioButtons("money_index", "Money index correction:", choices = c("No correction", "EuroStat '19")) ),
                        column(4, 
                               sliderInput("slapnea_prevalence_male", label = "Male Sleep Apnea (AHI≥15) prevalence", min = 1, max = 85, value = 60, step = .01)),
                        column(4, 
                               downloadButton("downloadData","Download data"),
                               downloadButton("downloadPlot", "Download plot")
                        )
                      ), # saadot 2
                      fixedRow(
                        # column(7,
                        rHandsontableOutput("hot")
                        # ) 
                      ) 
               ),
               column(6,
                      #kuvio
                      plotOutput("plot")
               )
             )
           )
  ),
  tabPanel("About",
           includeMarkdown("application_structure.md")
  ) #,
  # tabPanel(img(src="img/alllogos.png", style="height:45px; width:auto; float:right; padding-right:25px"))
)

