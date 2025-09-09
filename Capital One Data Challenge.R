## Capital One Data Challenge ##
# Declan Murphy
# DUE: 09/13/2025

# Step 0: Set Up Environment
library(tidyverse)
library(readxl)

# Step 1A: Read in Source Data
# All file paths are relative to the working directory

Flights <- read.csv('./data/Flights.csv')
Tickets <- read.csv('./data/Tickets.csv')
Airport_Codes <- read.csv("./data/Airport_Codes.csv")

Metadata <- read_xlsx('./data/Airline_Challenge_Metadata.xlsx')

# Step 1B: Clean Data

Flights$DISTANCE <- as.numeric(Flights$DISTANCE)
Flights$AIR_TIME <- as.numeric(Flights$AIR_TIME)
Flights <- Flights %>%
  filter(CANCELLED == 0.00) # Filter to only non-cancelled flights

# Step 2: Record Assumptions and Scalar Values

# Per the instructions, we can assume the following (scalar) values for use later one:
Fuel_Oil_Maint_Crew_per_Mile <- 8.00
Depreciation_Insurance_Other_per_Mile <- 1.18
Medium_Airport_Cost <- 5000
Large_Airport_Cost <- 10000


# TASK 1: What are the 10 BUSIEST ROUND TRIP ROUTES (excl. cancelled flights) for 1Q2019?
# 1A: Aggregate to Round Trips

Flights_In <- Flights %>%
  mutate(match_key = paste0(OP_CARRIER, ORIGIN, DESTINATION)) %>%
  group_by(match_key) %>%
  mutate(total_dep_delay = sum(DEP_DELAY),
  total_arr_delay = sum(ARR_DELAY),
  total_air_time = sum(AIR_TIME),
  route_distance = median(DISTANCE),
  mean_occupancy_rate = mean(OCCUPANCY_RATE),
  total_flights_route = n())

Flights_Out <- Flights %>%
  mutate(match_key = paste0(OP_CARRIER, DESTINATION, ORIGIN))%>%
  group_by(match_key) %>%
  mutate(total_dep_delay = sum(DEP_DELAY),
  total_arr_delay = sum(ARR_DELAY),
  total_air_time = sum(AIR_TIME),
  route_distance = median(DISTANCE),
  mean_occupancy_rate = mean(OCCUPANCY_RATE),
  total_flights_route = n())

Round_Trip <- inner_join(Flights_In, Flights_Out, by = "match_key", suffix = c("_In", "_Out"))


# 1B: Select Busiest from Round Trips

Busiest_Routes <- Round_Trip %>%
  group_by(ORIGIN) %>% # Placeholder for round trip
  summarize(trips = n()) %>%
  arrange(desc(trips)) %>%
  slice_head(n = 10)

# TASK 2: Top 10 Most Profitable Routes?
Round_Trip <- Round_Trip %>%
  mutate(Per_Mile_Cost = (DISTANCE * Fuel_Oil_Maint_Crew_per_Mile) + (DISTANCE * Depreciation_Insurance_Other_per_Mile)) %>%
  mutate(Delay_Cost = if_else(DEP_DELAY > 15, ((DEP_DELAY - 15) * 75), 0)) 
