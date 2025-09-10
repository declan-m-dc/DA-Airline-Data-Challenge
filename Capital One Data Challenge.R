## Capital One Data Challenge ##
# Declan Murphy
# DUE: 09/13/2025


# SETUP #### 

# Step 0.1: Set Up Environment
library(tidyverse)
library(readxl)

# Step 0.2: Read In Source Data
# All file paths are relative to the working directory

Flights <- read.csv('./data/Flights.csv', strip.white = TRUE)
Tickets <- read.csv('./data/Tickets.csv', strip.white = TRUE)
Airport_Codes <- read.csv("./data/Airport_Codes.csv", strip.white = TRUE)

Metadata <- read_xlsx('./data/Airline_Challenge_Metadata.xlsx')

# Step 0.3: Clean Data

Flights <- Flights %>%
  filter(CANCELLED == 0.00) %>%
  mutate(DISTANCE = suppressWarnings(as.numeric(DISTANCE)),
         AIR_TIME = suppressWarnings(as.numeric(AIR_TIME))) %>%
  mutate(Normalized_Dep_Delay = if_else(DEP_DELAY > 15,
                                        DEP_DELAY - 15,
                                        0),
         Normalized_Arr_Delay = if_else(ARR_DELAY > 15,
                                        ARR_DELAY - 15,
                                        0))

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
         Destination_Airport_Type = TYPE.y)

# Step 0.4: Record Assumptions and Scalar Values

# Per the instructions, we can assume the following (scalar) values for use later one:
Fuel_Oil_Maint_Crew_per_Mile <- 8.00
Depreciation_Insurance_Other_per_Mile <- 1.18
Medium_Airport_Cost <- 5000
Large_Airport_Cost <- 10000

# TASKS ####

# TASK 1: What are the 10 BUSIEST ROUND TRIPS? #####

Tickets_with_Airport <- Tickets_with_Airport %>%
  mutate(Route = paste0(pmin(ORIGIN, DESTINATION),
                        pmax(ORIGIN, DESTINATION))) %>%
  mutate(Airport_Cost = case_when(
    Origin_Airport_Type == "medium_airport" & Destination_Airport_Type == "medium_airport" ~ 10000,
    Origin_Airport_Type == "medium_airport" & Destination_Airport_Type == "large_airport" ~ 15000,
    Origin_Airport_Type == "large_airport" & Destination_Airport_Type == "medium_airport" ~ 15000,
    Origin_Airport_Type == "large_airport" & Destination_Airport_Type == "large_airport" ~ 20000,
    .default = 0
  ))

# Use of pmin() and pmax() allows for identical pairs for Origin A -> Dest B and
# Origin B -> Dest A

Round_Trip_Summary <- Tickets_with_Airport %>%
  group_by(Route) %>%
  summarize(total_passengers = sum(PASSENGERS, na.rm = TRUE),
            total_fares = sum(ITIN_FARE, na.rm = TRUE),
            route_airport_cost = mean(Airport_Cost)) %>%
  arrange(desc(total_passengers))

Busiest_Round_Trips <- Round_Trip_Summary %>%
  ungroup() %>%
  slice_max(n=10, order_by = total_passengers)
  

# TASK 2: What are the 10 most profitable routes? #####

Flights_ <- Flights %>%
  mutate(Route = paste0(pmin(ORIGIN, DESTINATION),
                        pmax(ORIGIN, DESTINATION))) %>%
  group_by(Route) %>%
  summarize(total_dep_delay = sum(Normalized_Dep_Delay, na.rm = TRUE),
            total_arr_delay = sum(Normalized_Arr_Delay, na.rm = TRUE),
            total_air_time = sum(AIR_TIME, na.rm = TRUE),
            route_distance = median(DISTANCE, na.rm = TRUE),
            route_occupancy_rate = round(sum(OCCUPANCY_RATE) / n(), 4),
            total_flights_route = n())

# Merge flight data with ticket data

Round_Trips_Summary_Expanded <- left_join(Round_Trip_Summary,
                                          Flights_,
                                          by = "Route")


Round_Trips_Summary_Expanded <- Round_Trips_Summary_Expanded %>%
  mutate(Per_Mile_Costs = (route_distance * Fuel_Oil_Maint_Crew_per_Mile * total_flights_route) 
         + (route_distance * Depreciation_Insurance_Other_per_Mile * total_flights_route)) %>%
  mutate(Delay_Costs = (total_dep_delay * 75) + (total_arr_delay) * 75) %>%
  mutate(Total_Airport_Cost = (route_airport_cost * total_flights_route)) %>%
  mutate(TOTAL_COSTS = Per_Mile_Costs + Delay_Costs + Total_Airport_Cost)

Round_Trips_Summary_Expanded <- Round_Trips_Summary_Expanded %>%
  mutate(Occupancy = (200 * route_occupancy_rate),
         Baggage_Revenue = (0.5 * Occupancy * 70 * total_flights_route)) %>%
  mutate(TOTAL_REVENUE = total_fares + Baggage_Revenue)

Round_Trips_Summary_Expanded <- Round_Trips_Summary_Expanded %>%
  mutate(Profit = TOTAL_REVENUE - TOTAL_COSTS) %>%
  arrange(desc(Profit))

Most_Profitable_Routes <- Round_Trips_Summary_Expanded %>%
  ungroup() %>%
  slice_max(Profit, n = 10)

# TASK 3: What five routes do you recommend investing in? ####

High_Occupancy <- Round_Trips_Summary_Expanded %>%
  filter(Occupancy > 100)

Profitable_Routes <- Round_Trips_Summary_Expanded %>%
  filter(Profit > 0,
         Route %in% High_Occupancy$Route) %>%
  slice_max(Profit, n = 5)

Chosen_Routes <- Profitable_Routes$Route

# TASK 4: How long to break even for each route? ####
Upfront_Cost <- 90000000

Chosen_Routes_Info <- Round_Trips_Summary_Expanded %>%
  filter(Route %in% Chosen_Routes) %>%
  ungroup() %>%
  mutate(Per_Trip_Profit = Profit / total_flights_route) %>%
  mutate(Trips_to_Profit = Upfront_Cost / Per_Trip_Profit)


# TASK 5: KPI's to track for future performance? ####

# On-Time Percentage -- how are we optimizing to minimize delays?
# Average Delay -- related to the above: when delays occur, how long are they?
# Occupancy Rate -- how full are these flights? How can they be better scheduled
# to maximize occupancy?
# Average Ticket Price -- what prices are consumers paying for these routes?
# Percent of Flights Cancelled -- what percentage of flights are cancelled?

