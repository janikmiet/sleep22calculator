age_filter <- "3069" # "1574" # "3069"
prev_simple %>% 
  filter(location_name == "Italy" & age_group == age_filter) %>% 
  group_by(condition) %>% 
  mutate(prevalence = ifelse(age_filter == "3069", ifelse(!is.na(ihme), ihme, prevalence_base_italy), prevalence_base_italy)) %>% ## condition prevalence first condition Armeni/Benjafield, second if ihme data available.
  # mutate(prevalence = ifelse(is.na(ihme), prevalence_base_italy, ihme)) %>% # condition prevalences comes from ihme, if available
  data.frame()


## Causes Prevalences for the selected country and age group -----
age_filter <-  "1574" # "3069"
loc <- "Italy"
prevalences <- prev %>% 
    filter(location_name == loc & age_group == age_filter) %>% ## changed
    mutate(prevalence = ifelse(is.na(ihme), prevalence_base_italy, ihme)) %>%
    data.frame()
head(prevalences)


## OSA value (sleep apnea prevalence) -----
osa_value <- osa$rate[osa$gender == "Both" & osa$var == "Moderate-Severe"] # osa value from the table armeni


## Core of calculus part 1 ----
## Calculate prevalent cases and costs per conditions
prevalences %>% 
    filter(location_name == loc) %>%
    mutate(
      ## PAF calculation for Risk Ratio or Odds Ratio:
      PAFRR = ifelse(!is.na(RR), (osa_value * (RR - 1) / (osa_value * (RR - 1) + 1)), NA), 
      PAFOR = ifelse(!is.na(OR), paf_or(OR, prevalence, osa_value), NA),
      PAF = ifelse(is.na(PAFOR), PAFRR, PAFOR),
      ## TODO extra rivit
      
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
  

# ## Make money correction if needed
  # if(input$money_index == "EuroStat '19"){
  #   dplot <- dplot %>% 
  #     left_join(money_correction, by = "location_name") %>% 
  #     mutate(corrected = ifelse(is.na(index), FALSE, TRUE),
  #            index = ifelse(is.na(index), 1, index),
  #            direct_cost = direct_cost * index,
  #            direct_non_healthcare_cost = direct_non_healthcare_cost * index,
  #            productivity_lost_cost = productivity_lost_cost * index,
  #            total_costs = total_costs * index) 
  # }
  return(dplot)
# })