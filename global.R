library(DBI)
library(duckdb)
library(tidyverse)
library(tidyr)
library(patchwork)
library(DT)

options(scipen = 999)

con <- dbConnect(duckdb::duckdb(), dbdir="sleep22_shiny.duckdb", read_only=TRUE)
dbListTables(conn = con)
# dbListFields(con, "prevalences")

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

## Population info
tbl(con, "popu_info") %>% 
  left_join(tbl(con, "pop"), by = "location_name") %>% 
  collect() -> popu_info

duckdb::dbDisconnect(con)

rm(list=c("con"))