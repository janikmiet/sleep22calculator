library(arrow)
library(tidyverse)
library(tidyr)
library(DT)
library(ggpubr)
library(shiny)
library(rhandsontable)
library(data.table)
library(markdown)

options("shiny.useragg" = TRUE)
options(scipen = 999)

## Load datasets -----
money_correction <- arrow::read_parquet("data/money_correction.parquet")
prev <- arrow::read_parquet("data/prevalences.parquet") 
osa <- arrow::read_parquet("data/osa.parquet")
osanew <- arrow::read_parquet("data/osanew.parquet")
pop <- arrow::read_parquet("data/pop.parquet")
money_correction <- arrow::read_parquet("data/money_correction.parquet")
locations <- prev %>% 
  group_by(location_name) %>% 
  summarise(1) %>% 
  collect() -> locations
locations <- sort(locations$location_name)

## TODO tallenna tämä excelistä suoraan, tee muutos sleep22-projektiin
prev_simple <- arrow::read_parquet("data/prevalences_simple.parquet") 

## PAF ODDS RATIO Function -----
## Give only decimals in parameters
# OR = Odds Ratio
# PD = having a disease, prevalence
# PE = exposed, sleep apnea prevalence / TODO accidents muuta  prevalenssia
# (PE_ =  unexposed)
paf_or <- function(OR, PD, PE){
  PD = PD * 100
  PE = PE * 100
  PE_ = 100 - PE
  VALUE1 = (PD * (1 - OR) + PE_ + OR * PE + sqrt( (PD * (1 - OR) + PE_ + OR * PE )^2 - 4 * PE_ * (1 - OR) *PD )) / (2 * PE_ * (1 - OR))
  VALUE2 = (PD * (1 - OR) + PE_ + OR * PE - sqrt( (PD * (1 - OR) + PE_ + OR * PE )^2 - 4 * PE_ * (1 - OR) *PD )) / (2 * PE_ * (1 - OR))
  VALUE <- ifelse(VALUE1 <= 100 & VALUE1 >= 0, VALUE1, VALUE2)
  PAF = 1 - ((100 * VALUE) / PD)
  return(PAF)
}




