## Capital One Data Challenge ##
# Declan Murphy
# DUE: 09/13/2025

# Step 0: Set Up Environment
library(tidyverse)
library(readxl)

# Step 1A: Read in Source Data
# All file paths are relative to the working directory

Flights <- read.csv('./data/Flights.csv', strip.white = TRUE)
Tickets <- read.csv('./data/Tickets.csv', strip.white = TRUE)
Airport_Codes <- read.csv("./data/Airport_Codes.csv", strip.white = TRUE)

Metadata <- read_xlsx('./data/Airline_Challenge_Metadata.xlsx')

# Step 1B: Clean Data

Flights$DISTANCE <- as.numeric(Flights$DISTANCE)
Flights$AIR_TIME <- as.numeric(Flights$AIR_TIME)
Flights <- Flights %>%
  filter(CANCELLED == 0.00) # Filter to only non-cancelled flights

Airport_Codes <- Airport_Codes %>%
  filter(TYPE == "medium_airport" | TYPE == "large_airport") %>% # Filter to medium and large airports
  filter(IATA_CODE %in% unique(IATA_CODE) & !is.na(IATA_CODE))


Tickets <- Tickets %>%
  filter(ROUNDTRIP == 1) %>% # Filter to only round trip tickets
  mutate(ITIN_FARE = replace_na(as.numeric(ITIN_FARE), 0))
  
Tickets_with_Airport <- left_join(Tickets, 
                                  Airport_Codes,
                                  by = c("ORIGIN" = "IATA_CODE"))

Tickets_with_Airport <- left_join(Tickets_with_Airport,
                                  Airport_Codes,
                                  by = c("DESTINATION" = "IATA_CODE"))

Tickets_with_Airport <- Tickets_with_Airport %>%
  select(1:12,
         Origin_Airport_Type = TYPE.x,
         Destination_Airport_Type = TYPE.y) %>%
  mutate(Route = paste0(ORIGIN, DESTINATION))

# Step 2: Record Assumptions and Scalar Values

# Per the instructions, we can assume the following (scalar) values for use later one:
Fuel_Oil_Maint_Crew_per_Mile <- 8.00
Depreciation_Insurance_Other_per_Mile <- 1.18
Medium_Airport_Cost <- 5000
Large_Airport_Cost <- 10000



# TASK 1: What are the 10 BUSIEST ROUND TRIPS?

Round_Trip_Summary <- Tickets_with_Airport %>%
  group_by(Route, Origin_Airport_Type, Destination_Airport_Type) %>%
  summarize(total_passengers = sum(PASSENGERS),
            total_fares = sum(ITIN_FARE)) %>%
  arrange(desc(total_passengers))

Busiest_Round_Trips <- Round_Trip_Summary %>%
  slice_max(n=10, order_by = total_passengers)
  

# TASK 2: What are the 10 most profitable routes?

Flights_Summary <- Flights %>%
  mutate(Route = paste0(ORIGIN, DESTINATION))








# TASK 1: What are the 10 BUSIEST ROUND TRIP ROUTES (excl. cancelled flights) for 1Q2019?
# 1A: Aggregate to Round Trips

Flights_In <- Flights %>%
  mutate(match_key = paste0(OP_CARRIER, ORIGIN, DESTINATION)) %>%
  group_by(match_key, OP_CARRIER, DESTINATION, ORIGIN) %>%
  summarize(total_dep_delay = sum(DEP_DELAY),
  total_arr_delay = sum(ARR_DELAY),
  total_air_time = sum(AIR_TIME),
  route_distance = median(DISTANCE),
  mean_occupancy_rate = mean(OCCUPANCY_RATE),
  total_flights_route = n())

Flights_Out <- Flights %>%
  mutate(match_key = paste0(OP_CARRIER, DESTINATION, ORIGIN))%>%
  group_by(match_key, OP_CARRIER, DESTINATION, ORIGIN) %>%
  summarize(total_dep_delay = sum(DEP_DELAY),
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
