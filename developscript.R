source("global.R")
loc <- "Italy"
slapnea_prevalence_female
slapnea_prevalence_male

##  Re-calculate sleep apnea (moderate/severe) prevalences per gender including slider inputs
dosa <- osanew
dosa$rate[dosa$gender == "Female" & dosa$var=="Moderate-Severe"] <- slapnea_prevalence_female / 100
dosa$rate[dosa$gender == "Male" & dosa$var=="Moderate-Severe"] <- slapnea_prevalence_male / 100
dosa$rate[dosa$gender == "Female" & dosa$var=="Moderate"] <- 0.5342 * (input$slapnea_prevalence_female / 100)
dosa$rate[dosa$gender == "Male" & dosa$var=="Moderate"] <-   0.4004  * (input$slapnea_prevalence_male / 100)
dosa$rate[dosa$gender == "Female" & dosa$var=="Severe"] <- 0.4658 * (input$slapnea_prevalence_female / 100)
dosa$rate[dosa$gender == "Male" & dosa$var=="Severe"] <-   0.5996 * (input$slapnea_prevalence_male / 100)

## Causes prevalences simple table ----
prevalences_simple <- prev_simple %>% 
  filter(location_name == loc) %>% 
  group_by(condition) %>% 
  mutate(prevalence = ifelse(is.na(ihme), prevalence_base_italy, ihme)) %>% 
  data.frame()
## Causes Prevalences for the selected country and age group -----
prevalences <- prev %>% 
    filter(location_name == loc) %>% ## changed
    mutate(prevalence = ifelse(is.na(ihme), prevalence_base_italy, ihme)) %>%
    data.frame()
# head(prevalences)
## Join prevalences simpe and prevalences
d <- prevalences_simple %>% 
  left_join(prevalences %>% select(condition, OSA_severity, gender, RR, OR), by = "condition") %>% 
  ## cases which has osa_severity = Overall to moderate-severe
  mutate(OSA_severity=ifelse(OSA_severity == "Overall", "Moderate-Severe",OSA_severity))

## OSA value (sleep apnea prevalence) -----
# osa_value <- osa$rate[osa$gender == "Both" & osa$var == "Moderate-Severe"] # osa value from the table armeni
d <- d %>% 
  left_join(osanew %>% filter(location_name == loc) %>%  rename(OSA_severity = var), by = c("OSA_severity", "gender"))


## Core of calculus part 1 ----
## Calculate prevalent cases and costs per conditions
d %>% 
    # filter(location_name == loc) %>%
    mutate(
      ## PAF calculation for Risk Ratio or Odds Ratio:
      PAFRR = ifelse(!is.na(RR), (rate * (RR - 1) / (rate * (RR - 1) + 1)), NA), 
      PAFOR = ifelse(!is.na(OR), paf_or(OR, prevalence, rate), NA),
      PAF = ifelse(is.na(PAFOR), PAFRR, PAFOR),
      ## TODO extra rivit
      
      ## Prevalents per conditions
      prevalent_cases = prevalence * pop_both, 
      prevalent_cases_influenced_osa = PAF * prevalent_cases,
      ## Costs per conditions
      direct_cost = prevalent_cases_influenced_osa * direct_healthcare_cost,
      direct_non_healthcare_cost = prevalent_cases_influenced_osa * direct_nonhealthcare_cost,
      productivity_lost_cost = prevalent_cases_influenced_osa * productivity_losses_cost
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