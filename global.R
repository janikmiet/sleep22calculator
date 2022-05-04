library(DBI)
library(arrow)
library(tidyverse)
library(tidyr)
library(patchwork)
library(DT)

library(shiny)
library(rhandsontable)
library(data.table)
library(hrbrthemes)
library(ragg)

options(scipen = 999)

money_correction <- arrow::read_parquet("data/money_correction.parquet")
prev <- arrow::read_parquet("data/prevalences.parquet") 
osa <- arrow::read_parquet("data/osa.parquet")
osanew <- arrow::read_parquet("data/osanew.parquet")
pop <- arrow::read_parquet("data/pop.parquet")
popu_info <- arrow::read_parquet("data/popu_info.parquet")
money_correction <- arrow::read_parquet("data/money_correction.parquet")
locations <- prev %>% 
  group_by(location_name) %>% 
  summarise(1) %>% 
  collect() -> locations
locations <- sort(locations$location_name)

# paf_or(OR, prevalence, osa_table()$rate[osa_table()$gender == "Both" & osa_table()$var == "Moderate-Severe"])
paf_or <- function(OR, PD, PE){
  PE_ = 100 - PE
  VALUE1 = (PD * (1 - OR) + PE_ + OR * PE + sqrt( (PD * (1 - OR) + PE_ + OR * PE )^2 - 4 * PE_ * (1 - OR) *PD )) / (2 * PE_ * (1 - OR))
  VALUE2 = (PD * (1 - OR) + PE_ + OR * PE - sqrt( (PD * (1 - OR) + PE_ + OR * PE )^2 - 4 * PE_ * (1 - OR) *PD )) / (2 * PE_ * (1 - OR))
  VALUE <- ifelse(VALUE1 < 100 & VALUE1 > 0, VALUE1, VALUE2)
  PAF = 1 - ((100 * VALUE) / PD)
  return(PAF)
}

if(FALSE){
  # DUCKDB version
  # con <- dbConnect(duckdb::duckdb(), dbdir="sleep22_shiny.duckdb", read_only=TRUE)
  # dbListTables(conn = con)
  # # dbListFields(con, "prevalences")
  # 
  # ## Full table prevalences
  # prev <- tbl(con, "prevalences") %>% 
  #   collect() 
  # 
  # ## Full osa table
  # osa <- tbl(con, "osa") %>%  collect()
  # osanew <- tbl(con, "osanew") %>%  collect()
  # 
  # ## Full population table
  # pop <- tbl(con, "pop") %>% collect()
  # 
  # ## Locations list
  # tbl(con, "prevalences") %>% 
  #   group_by(location_name) %>% 
  #   summarise(1) %>% 
  #   collect() -> locations
  # locations <- sort(locations$location_name)
  # 
  # ## Population info
  # tbl(con, "popu_info") %>% 
  #   left_join(tbl(con, "pop"), by = "location_name") %>% 
  #   collect() -> popu_info
  # 
  # ## Money index
  # tbl(con, "money_correction") %>% 
  #   collect() -> money_correction
  # 
  # ## Disconnect
  # duckdb::dbDisconnect(con)
  # 
  # rm(list=c("con"))
}