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
Flights$Normalized_Dep_Delay <- if_else(Flights$DEP_DELAY > 15,
                                        Flights$DEP_DELAY - 15,
                                        0)
Flights$Normalized_Arr_Delay <- if_else(Flights$ARR_DELAY > 15,
                                        Flights$ARR_DELAY - 15,
                                        0)

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
  summarize(total_passengers = sum(PASSENGERS, na.rm = TRUE),
            total_fares = sum(ITIN_FARE, na.rm = TRUE)) %>%
  arrange(desc(total_passengers))

Busiest_Round_Trips <- Round_Trip_Summary %>%
  ungroup() %>%
  slice_max(n=10, order_by = total_passengers)
  

# TASK 2: What are the 10 most profitable routes?

Flights_Summary <- Flights %>%
  mutate(Route = paste0(ORIGIN, DESTINATION),
         Route2 = paste0(DESTINATION, ORIGIN)) %>%
  group_by(Route) %>%
  summarize(total_dep_delay = sum(Normalized_Dep_Delay, na.rm = TRUE),
            total_arr_delay = sum(Normalized_Arr_Delay, na.rm = TRUE),
            total_air_time = sum(AIR_TIME, na.rm = TRUE),
            route_distance = median(DISTANCE, na.rm = TRUE),
            occupancy_rate = round(sum(OCCUPANCY_RATE) / n(), 4),
            total_flights_route = n())

# Need to duplicate inbound and outbound?

Round_Trips_Summary_Expanded <- left_join(Round_Trip_Summary,
                                          Flights_Summary,
                                          by = "Route")


Round_Trips_Summary_Expanded <- Round_Trips_Summary_Expanded %>%
  mutate(Per_Mile_Costs = (route_distance * Fuel_Oil_Maint_Crew_per_Mile * total_flights_route) 
         + (route_distance * Depreciation_Insurance_Other_per_Mile * total_flights_route)) %>%
  mutate(Delay_Costs = (total_dep_delay * 75) + (total_arr_delay) * 75) %>%
  mutate(Airport_Cost_Origin = case_when(Origin_Airport_Type == "medium_airport" ~ 5000,
                                         Origin_Airport_Type == "large_airport" ~ 10000),
         Airport_Cost_Destination = case_when(Destination_Airport_Type == "medium_airport" ~ 5000,
                                              Destination_Airport_Type == "large_airport" ~ 10000),
         Total_Airport_Cost = Airport_Cost_Origin + Airport_Cost_Destination) %>%
  mutate(TOTAL_COSTS = Per_Mile_Costs + Delay_Costs + Total_Airport_Cost)

Round_Trips_Summary_Expanded <- Round_Trips_Summary_Expanded %>%
  mutate(Occupancy = (200 * occupancy_rate),
         Baggage_Revenue = (0.5 * Occupancy * 70 * total_flights_route)) %>%
  mutate(TOTAL_REVENUE = total_fares + Baggage_Revenue)

Round_Trips_Summary_Expanded <- Round_Trips_Summary_Expanded %>%
  mutate(Profit = TOTAL_REVENUE - TOTAL_COSTS) %>%
  arrange(desc(Profit))

Most_Profitable_Routes <- Round_Trips_Summary_Expanded %>%
  ungroup() %>%
  slice_max(Profit, n = 10)
