library(DBI)
library(duckdb)
library(tidyverse)
library(tidyr)
library(patchwork)
library(DT)

options(scipen = 999)

con <- dbConnect(duckdb::duckdb(), dbdir="sleep22_shiny.duckdb", read_only=TRUE)
# con <- dbConnect(duckdb::duckdb(), dbdir="sleep22_calculator.duckdb", read_only=TRUE)
dbListTables(conn = con)
# dbListFields(con, "prevalences")

## Input table testiin
# tbl(con, "prevalences") %>% 
#   # filter(location_name == "Finland") %>% 
#   # select(location_name, condition, type, ihme, prevalence_base_italy, annual_direct_healthcare_cost, annual_direct_nonhealthcare_cost, annual_productivity_losses_cost) %>% 
#   mutate(prevalence = ifelse(is.na(ihme), prevalence_base_italy, ihme)) %>% 
#   # select(location_name, condition, type, prevalence, annual_direct_healthcare_cost, annual_direct_nonhealthcare_cost, annual_productivity_losses_cost) %>% 
#   collect() -> df

## Full table prevalences
prev <- tbl(con, "prevalences") %>% 
  collect() 

## Full osa table
osa <- tbl(con, "osa") %>%  collect()
osanew <- tbl(con, "osanew") %>%  collect()

## Full population table
pop <- tbl(con, "pop") %>% collect()

## Locations list
tbl(con, "prevalences") %>% 
  group_by(location_name) %>% 
  summarise(1) %>% 
  collect() -> locations
locations <- sort(locations$location_name)

duckdb::dbDisconnect(con)

rm(list=c("con"))