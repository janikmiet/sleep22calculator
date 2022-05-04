library(shiny)
library(rhandsontable)
library(data.table)

shinyServer(function(input, output, session) {

  ## Prevalences with selected country
  prevalences <- reactive({
    
    ## TODO new data where is also age_group column, which is associated with prevalence
    # age_filter <- ifelse(input$osa_selected == "Armeni et al.", "1574", "3069")
    
    prev %>% 
      # filter(location_name == input$location & age_group == age_filter) %>%  ## THIS is for the age filter
      filter(location_name == input$location) %>% 
      mutate(prevalence = ifelse(is.na(ihme), prevalence_base_italy, ihme),
             #  population (Armeni 15-74 or Benjafield 30-69) depending on OSA 
             pop_both = ifelse(input$osa_selected == "Armeni et al.",  pop_1574_both, pop_3069_both),
             pop_female= ifelse(input$osa_selected == "Armeni et al.",  pop_1574_female, pop_3069_female),
             pop_male= ifelse(input$osa_selected == "Armeni et al.",  pop_1574_male, pop_3069_male)) %>% 
      data.frame()
  })
  
  ## OSA table selected
  osa_table <- reactive({
    if(input$osa_selected == "Armeni et al."){
      d <- osa
    }else{
      d <- osanew %>% filter(location_name == input$location)
    }
    return(d)
  })
  
  ## Slider update
  toListen <- reactive({
    list(input$location, input$osa_selected)
  })
  
  observeEvent(toListen() ,{
    ## Female slider update
    val_female <- osa_table() %>% 
      filter(gender == "Female" & (var == "Moderate-Severe") ) %>% 
      group_by(1) %>% 
      summarise(rate = sum(rate)) 
    val_female <- val_female$rate * 100
    ## Update
    updateSliderInput(
      session = session,
      inputId = "slapnea_prevalence_female",
      value = val_female
    )
    ## Male Slider update
    val_male <- osa_table() %>%
      filter(gender == "Male" & (var == "Moderate-Severe") ) %>%
      group_by(1) %>%
      summarise(rate = sum(rate))
    val_male <- val_male$rate * 100
    ## Update
    updateSliderInput(
      session = session,
      inputId = "slapnea_prevalence_male",
      value = val_male
    )
  })
  
  ## osa value
  osa_value <- reactive({
    val <- osa_table()$rate[osa_table()$gender == "Both" & osa_table()$var == "Moderate-Severe"]
    return(val)
  })
  
  ## Core of calculus part 1 ----
  calc_total = reactive({
    ## TODO add mortality to calc

    ## Get original data and enchance it with hot table
    d <- prevalences() %>% 
      rows_update(hot_to_r(input$hot), by = "condition")
    
    ## Calculate prevalent cases and costs per conditions
    d %>% 
      filter(location_name == input$location) %>%
      mutate(
        ## PAF formula from TODO ADD SOURCE. Is it specific for Armeni?
        PAFRR = ifelse(!is.na(RR), (prevalence * (RR - 1) / (prevalence * (RR - 1) + 1)), NA), 
        # TODO here PAF calculation for OR
        PAFOR = ifelse(!is.na(OR), paf_or(OR, prevalence, osa_value()), NA),
        # PAF = ifelse(is.na(PAFOR), PAFRR, PAFOR),
        ## Prevalents per conditions
        prevalent_cases = prevalence * pop_both, 
        prevalent_cases_influenced_osa = PAF * prevalent_cases,
        ## Costs per conditions
        direct_cost = prevalent_cases_influenced_osa * annual_direct_healthcare_cost,
        direct_non_healthcare_cost = prevalent_cases_influenced_osa * annual_direct_nonhealthcare_cost,
        productivity_lost_cost = prevalent_cases_influenced_osa * annual_productivity_losses_cost
      ) %>%
      mutate(direct_cost = ifelse(is.na(direct_cost), 0 , direct_cost),
             direct_non_healthcare_cost = ifelse(is.na(direct_non_healthcare_cost), 0 , direct_non_healthcare_cost),
             productivity_lost_cost = ifelse(is.na(productivity_lost_cost), 0 , productivity_lost_cost),
             total_costs = direct_cost + direct_non_healthcare_cost + productivity_lost_cost) -> dplot
    
    ## Make money correction
    if(input$money_index == "EuroStat"){
      dplot <- dplot %>% 
        left_join(money_correction, by = "location_name") %>% 
        mutate(corrected = ifelse(is.na(index), FALSE, TRUE),
               index = ifelse(is.na(index), 1, index),
               direct_cost = direct_cost * index,
               direct_non_healthcare_cost = direct_non_healthcare_cost * index,
               productivity_lost_cost = productivity_lost_cost * index,
               total_costs = total_costs * index) 
    }
    
    return(dplot)
    })
    
  ## Core of calculus part 2 ----
    calc = reactive({
      ## OSA-table and re-calculus
      dosa <- osa_table()
      ##  Re-calculate sleep apnea prevalences per gender by slider inputs
      dosa$rate[dosa$gender == "Female" & dosa$var=="Moderate-Severe"] <- input$slapnea_prevalence_female / 100
      dosa$rate[dosa$gender == "Male" & dosa$var=="Moderate-Severe"] <- input$slapnea_prevalence_male / 100
      ## TODO CHECK these correction values with comments
      dosa$rate[dosa$gender == "Female" & dosa$var=="Moderate"] <- 0.5342 * (input$slapnea_prevalence_female / 100)
      dosa$rate[dosa$gender == "Male" & dosa$var=="Moderate"] <-   0.4004  * (input$slapnea_prevalence_male / 100)
      dosa$rate[dosa$gender == "Female" & dosa$var=="Severe"] <- 0.4658 * (input$slapnea_prevalence_female / 100)
      dosa$rate[dosa$gender == "Male" & dosa$var=="Severe"] <-   0.5996 * (input$slapnea_prevalence_male / 100)
      
      ## Calculate costs (total & patient) per locations
      calc_total() %>%
        group_by(location_name, pop_female, pop_male) %>%
        summarise(direct_cost = sum(direct_cost, na.rm = T),
                  direct_non_healthcare_cost = sum(direct_non_healthcare_cost, na.rm = T),
                  productivity_lost_cost = sum(productivity_lost_cost, na.rm = T)) %>%
        mutate(
          ## Absolute values with separated moderate/severe calculation
          absolute_value_severe_moderate = ( (pop_female * dosa$rate[dosa$var == "Moderate" & dosa$gender == "Female"]) + (pop_female * dosa$rate[dosa$var == "Severe" & dosa$gender == "Female"]) + (pop_male * dosa$rate[dosa$var == "Moderate" & dosa$gender == "Male"]) + (pop_male * dosa$rate[dosa$var == "Severe" & dosa$gender == "Male"])),
          absolute_value_mild = ( pop_female * dosa$rate[dosa$var == "Mild" & dosa$gender == "Female"] + pop_male * dosa$rate[dosa$var == "Mild" & dosa$gender == "Male"] ),
          ## Costs per patients
          patient_direct_cost = direct_cost / absolute_value_severe_moderate,
          patient_nonhealthcare_cost = direct_non_healthcare_cost / absolute_value_severe_moderate,
          patient_productivity_cost = productivity_lost_cost / absolute_value_severe_moderate,
          patient_total_cost = patient_direct_cost + patient_nonhealthcare_cost + patient_productivity_cost
        ) -> dplot
      
    return(dplot)
  })
  
  ## Hot table output ----
  output$hot <- renderRHandsontable({
    rhandsontable(
      data = prevalences()[, c("condition", "prevalence", "annual_direct_healthcare_cost", "annual_direct_nonhealthcare_cost", "annual_productivity_losses_cost")],
      rowHeaders = NULL) %>% 
      hot_col("condition", readOnly = TRUE) %>%
      # hot_col("type", readOnly = TRUE) %>% 
      hot_col("prevalence", format = "0%") %>% 
      hot_cell(1, "annual_direct_healthcare_cost", readOnly = TRUE) %>% 
      # hot_col("annual_direct_healthcare_cost", format = "0,0.00 €") %>%
      hot_cell(1, "annual_direct_nonhealthcare_cost", readOnly = TRUE) %>% 
      hot_cell(1, "annual_productivity_losses_cost", readOnly = TRUE) %>% 
      hot_cell(2, "annual_direct_healthcare_cost", readOnly = TRUE) %>% 
      hot_cell(2, "annual_direct_nonhealthcare_cost", readOnly = TRUE) %>% 
      hot_cell(2, "annual_productivity_losses_cost", readOnly = TRUE)
  })
  
  ## Table output
  ## For testing the output dataset 
  # output$textOuput <- renderText({
  #   paste(
  #     as.data.frame(hot_to_r(input$hot))
  #     # hot_to_r(input$hot)
  #     # unlist(hot_to_r(input$hot))#,
  #     # collapse = '; '
  #   )
  # })  
  
  ## Download -----
  
  ## For downloading CSV file
  makeQuery <- reactive({
    calc_total() %>% 
      select(location_name, cause_name, condition, pop_both, OR, RR, PAF, PAFRR, PAFOR, prevalence, prevalent_cases, prevalent_cases_influenced_osa, annual_direct_healthcare_cost, annual_direct_nonhealthcare_cost, annual_productivity_losses_cost,  direct_cost, direct_non_healthcare_cost, productivity_lost_cost, total_costs)
  })
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$location, "_sleep_apnea_cost_calculation-", ".csv", sep="")
    },
    content = function(file) {
      write.csv(makeQuery(), file)
    }
  )
  
  ## DT summary table
  output$summary = DT::renderDataTable({
    makeQuery()
  })
  
  ## For downloading the plot
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste(input$location, "_sleep_apnea_costs", ".png", sep="")
    },
    content = function(file) {
      png(file=file)
      plot(infograph())
      dev.off()
    }
  )
  
  ## Creating the infograph ----
  infograph <- reactive({
    if (!is.null(calc())) {
      ## Donut plot output 
      calc_total() %>% # slapnea_cost1 %>% 
        summarise(
          direct = sum(direct_cost, na.rm = T),
          nonhealthcare = sum(direct_non_healthcare_cost, na.rm = T),
          productivity_lost = sum(productivity_lost_cost, na.rm = T)
        ) %>% 
        pivot_longer(c(direct, nonhealthcare, productivity_lost)) -> dat
      # Compute percentages
      dat$fraction <- dat$value / sum(dat$value)
      # Compute the cumulative percentages (top of each rectangle)
      dat$ymax <- cumsum(dat$fraction)
      # Compute the bottom of each rectangle
      dat$ymin <- c(0, head(dat$ymax, n=-1))
      # Compute label position
      dat$labelPosition <- (dat$ymax + dat$ymin) / 2
      # Compute a good label
      dat$label <- paste0(dat$name, "\n ", format(round(dat$value, -4), big.mark = "," ), " €")
      # Title
      label_title <- paste0(" Sleep Apnea Cost in ", input$location)
      label_subtitle <- paste0("Total Cost: ", format(round(sum(dat$value), -4), big.mark = ","), " €")
      # Donut plot
      p1 <- ggplot(dat, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=name)) +
        geom_rect() +
        geom_label( x=3.5, aes(y=labelPosition, label=label), size=4) +
        scale_fill_brewer(palette="Set2") +
        coord_polar(theta="y") +
        xlim(c(2, 4)) +
        # hrbrthemes::theme_ipsum(grid = F, axis_text_size = F ) +
        # theme(legend.position = "none") +
        theme(panel.background = element_rect(fill = "white"),
              legend.position = "none",
              panel.grid = element_blank(),
              axis.title = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_blank()) +
        labs(title = label_title, subtitle =label_subtitle, x="", y="")
      
      ## Barplot output
      dplot <- calc() %>%
        select(location_name, patient_direct_cost, patient_nonhealthcare_cost, patient_productivity_cost) %>%
        tidyr::pivot_longer(cols = c("patient_direct_cost", "patient_nonhealthcare_cost", "patient_productivity_cost"), names_to = "type", values_to = "euros") %>% 
        arrange(desc(type)) %>% group_by(location_name) %>% mutate(pos = cumsum(euros) - euros/2)
      
      p2 <- ggplot(data = dplot) +
        geom_bar(aes(x = location_name, y = euros, fill = type), stat = "identity") +
        geom_text(aes(x=location_name, y=sum(dplot$euros), label = paste0(round(sum(dplot$euros), -1), " €")), vjust=-.5) +
        geom_text(aes(x=location_name, y=pos, label = paste0(prettyNum(round(euros, -1),big.mark = ","), " €"))
                  , vjust = 0,  size = 4) +
        scale_fill_brewer(palette = "Set2", labels=c('Direct healthcare cost', 'Direct non-helthcare cost', 'Productivity losses')) +
        scale_y_continuous(limits = c(0, sum(dplot$euros) + 200)) +
        # hrbrthemes::theme_ipsum() +
        theme(plot.caption = element_text(hjust = 0, face= "italic"), #Default is hjust=1
              plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
              plot.caption.position =  "plot",
              legend.position = "right") +
        labs(x="", fill="", subtitle = "Per patient")
      
     p1 / p2
     
    }
  })
  
  ## Plot output
  output$plot = renderPlot(width = 450 , height = 700, {
    infograph()
  })
  
  infograph2 <- reactive({
    ## Population info
    dpop <- popu_info %>% 
      filter(location_name == input$location) %>% 
      mutate(
        Population = pop_both,
        Selected = pop_both - pop_1574_both,
        Affected = (input$slapnea_prevalence_male / 100) * pop_male + (input$slapnea_prevalence_female / 100) * pop_female) %>%
      select(location_name, Population, Selected, Affected) %>% 
      pivot_longer(c(Population, Selected, Affected))
    
    p3 <- ggplot(data = dpop) +
      geom_bar(aes(x=reorder(name, -value), y=value, fill = name), stat = "identity") +
      geom_label(aes(x=reorder(name, -value), y=value, label = paste0(name, " \n ", format(round(value,-4), big.mark = ",")))) +
      scale_fill_brewer(palette="Set3") +
      scale_y_continuous(limit = c(0, max(dpop$value) * 1.1)) +
      labs(x="", y="") +
      hrbrthemes::theme_ipsum() +
      theme(legend.position = "none") 
    
    p3
  })
  ## Population plot
  output$plot2 = renderPlot(width = 450 , height = 700, {
    infograph2()
  })

  
})