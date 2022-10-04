## Core of calculus part 1 ----

# joudun tekemään muutoksia laskentaan
# pitää otaa osa-arvo mukaan taulukkoon omaksi sarakkeeksi, ehtona on moderate-severe-arvo

## prevalent cases, prevalent cases influences by OSA, total costs and money correction
calc_total = reactive({
  ## Get original data and update it with hot table input
  d <- prev_simple %>%  # muutosta
    filter(location_name == "Italy" & age_group == "1574") # muutosta
    # rows_update(hot_to_r(input$hot), by = "condition") %>% 
    # # mutate(OR = NULL,
    #        RR = NULL
    # )
  ## Combine simple table and wider table 
  d <- prev %>% # muutosta
    filter(location_name == "Italy" & age_group == "1574") # muutosta
    select(condition, OSA_severity, RR, OR) %>% 
    left_join(d, by = "condition") 
    
  ## Lisää OSA arvo jokaiselle riville  
    
  ## Calculate prevalent cases and costs per conditions
  d %>% 
    filter(location_name == input$location) %>%
    mutate(
      ## PAF calculation for Risk Ratio or Odds Ratio:
      PAFRR = ifelse(!is.na(RR), (osa_value() * (RR - 1) / (osa_value() * (RR - 1) + 1)), NA),
      PAFOR = ifelse(!is.na(OR), paf_or(OR, prevalence, osa_value()), NA),
      PAF = ifelse(is.na(PAFOR), ifelse(!is.na(PAFRR), PAFRR, 0), PAFOR),
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
  ## Make money correction if needed
  if(input$money_index == "EuroStat '19"){
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