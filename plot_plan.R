library(tidyverse)
library(tidyr)
library(duckdb)
library(ggplot2)

options(scipen = 999)

## DATA ----
loc <- "Italy"

## Connection
source("global.R")

## OSA table
osa <- osanew %>% filter(location_name == loc)


## Calculate prevalent cases and costs per conditions & locations
prev %>% 
  filter(location_name == loc & age_group == "1574") %>% 
  mutate(
    # Select prevalence which to use
    prevalence = ifelse(is.na(ihme), prevalence_base_italy, ihme), 
    ## PAF
    PAF = ifelse(!is.na(RR), (prevalence * (RR - 1) / (prevalence * (RR - 1) + 1)), PAF), 
    PAFOR = ifelse(!is.na(OR), paf_or(OR, prevalence, osanew$rate[osanew$gender == "Both" & osanew$var == "Moderate-Severe" & osanew$location_name == loc]), PAF),
    ## Prevalents per conditions
    prevalent_cases = prevalence * pop_both, ## Taudin prevalenssi * populaatio, ok
    prevalent_cases_influenced_osa = PAF * prevalent_cases, ## PAF * prevalent_cases, ok
    ## Costs per conditions
    direct_cost = prevalent_cases_influenced_osa * annual_direct_healthcare_cost, ## ok
    direct_non_healthcare_cost = prevalent_cases_influenced_osa * annual_direct_nonhealthcare_cost, ## ok
    productivity_lost_cost = prevalent_cases_influenced_osa * annual_productivity_losses_cost ## ok
    ) %>% 
  ## NA's to zero
  mutate(direct_cost = ifelse(is.na(direct_cost), 0 , direct_cost),
         direct_non_healthcare_cost = ifelse(is.na(direct_non_healthcare_cost), 0 , direct_non_healthcare_cost),
         productivity_lost_cost = ifelse(is.na(productivity_lost_cost), 0 , productivity_lost_cost),
         total_costs = direct_cost + direct_non_healthcare_cost + productivity_lost_cost) -> slapnea_cost1

## Calculate total & patient costs per locations
slapnea_cost1 %>% 
  group_by(location_name, pop_female, pop_male) %>% 
  summarise(
    ## Sums of costs
    direct_cost = sum(direct_cost, na.rm = T),
    direct_non_healthcare_cost = sum(direct_non_healthcare_cost, na.rm = T),
    productivity_lost_cost = sum(productivity_lost_cost, na.rm = T)
    )  %>% 
  mutate(
    ## Absolute values for dividing the cost per patient
    absolute_value_severe_moderate = ( (pop_female * osa$rate[osa$var == "Moderate" & osa$gender == "Female"]) + (pop_female * osa$rate[osa$var == "Severe" & osa$gender == "Female"]) + (pop_male * osa$rate[osa$var == "Moderate" & osa$gender == "Male"]) + (pop_male * osa$rate[osa$var == "Severe" & osa$gender == "Male"])), ## ok
    # absolute_value_mild = ( pop_female * osa$rate[osa$var == "Mild" & osa$gender == "Female"]) + (pop_male * osa$rate[osa$var == "Mild" & osa$gender == "Male"] ), ## ok
    ## Costs per patients
    ## TODO THESE ARE WRONG IN EXCEL WHICH absolute value to use
    patient_direct_cost = direct_cost / absolute_value_severe_moderate, ## ok
    patient_nonhealthcare_cost = direct_non_healthcare_cost / absolute_value_severe_moderate, ## ok
    patient_productivity_cost = productivity_lost_cost / absolute_value_severe_moderate, ## ok
    ## Total cost per patient
    patient_total_cost = patient_direct_cost + patient_nonhealthcare_cost + patient_productivity_cost ## ok
  ) -> slapnea_cost2



## DONUT GRAPH ----

slapnea_cost1 %>% 
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
label_title <- paste0("Total cost ", format(round(sum(dat$value), -4), big.mark = ","), " €")

# Make the plot
p1 <- ggplot(dat, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=name)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=3) +
  scale_fill_brewer(palette="Set2") +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  # hrbrthemes::theme_ipsum(grid = F, axis_text_size = F ) +
  theme(legend.position = "none") +
  labs(title = label_title, x="", y="")
p1
## BAR GRAPH -----

# names(dplot_summary)
## To long format
dplot <- slapnea_cost2 %>% 
  select(location_name, patient_direct_cost, patient_nonhealthcare_cost, patient_productivity_cost) %>% 
  tidyr::pivot_longer(cols = c("patient_direct_cost", "patient_nonhealthcare_cost", "patient_productivity_cost"), names_to = "type", values_to = "euros")  %>% 
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
          legend.position = "bottom") +
    labs(x="", fill="", subtitle = "Per patient")

p2

## AFFECTED POPULATION PLOT ----
popu_info %>% 
  left_join(pop, by = "location_name") -> dpop

dpop <- dpop %>% 
  filter(location_name == loc) %>% 
  mutate(
    Population = pop_both,
    Selected = pop_both - pop_1574_both,
    Affected = 0.212 * pop_male + 0.124 * pop_female) %>% ## Numbers are random here, in application we use OSA-sliders
  select(location_name, Population, Selected, Affected) %>% 
  pivot_longer(c(Population, Selected, Affected))

p3 <- ggplot(data = dpop) +
  geom_bar(aes(x=reorder(name, -value), y=value, fill = name), stat = "identity") +
  geom_label(aes(x=reorder(name, -value), y=value, label = paste0(name, " \n ", format(round(value,-4), big.mark = ",")))) +
  scale_fill_brewer(palette="Set3") +
  scale_y_continuous(limit = c(0, max(dpop$value) * 1.1)) +
  labs(x="", y="") +
  # hrbrthemes::theme_ipsum() +
  theme(legend.position = "none") 

p3

## Put together -----
# install.packages("ggpubr")
library("ggpubr")

ggarrange(
  p3,                # First row with line plot
  # Second row with box and dot plots
  ggarrange(p1, p2, ncol = 2, labels = c("B", "C"), common.legend = TRUE, legend = "bottom"), 
  nrow = 2, 
  labels = "A"       # Label of the line plot
) 

ggarrange(
  # Second row with box and dot plots
  ggarrange(p1, p2, ncol = 2, labels = c("", ""), common.legend = TRUE, legend = "bottom"), 
  p3,                # First row with line plot
  nrow = 2, 
  labels = ""       # Label of the line plot
) 

library(patchwork)
p1 / p2
(p1 + p2) / p3

