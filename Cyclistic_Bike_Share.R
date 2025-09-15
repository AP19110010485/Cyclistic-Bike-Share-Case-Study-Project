# 🚴 Cyclistic Bike-Share Case Study
# Author: Souvik Mandal
# Date: Sys.Date()
# ---------------------------------------

# 📦 Load required libraries
install.packages(c(
  "tidyverse", "lubridate", "janitor", "skimr",
  "readr", "hms", "plotly", "ggthemes", "scales",
  "data.table", "knitr", "kableExtra"
))

library(tidyverse)
library(lubridate)
library(janitor)
library(skimr)
library(ggplot2)
library(hms)
library(plotly)
library(ggthemes)
library(scales)
library(knitr)
library(kableExtra)

# -------------------------------------------------
# 1. 📌 Business Problem
# Cyclistic wants to understand how annual members
# and casual riders use bikes differently, so the 
# marketing team can design strategies to convert
# casual riders into members.
# -------------------------------------------------

# -------------------------------------------------
# 2. 🗂 Load & Clean Data
# -------------------------------------------------
divvy_2019 <- read_csv("Divvy_Trips_2019_Q1.csv")
divvy_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")

# Clean 2019 data
divvy_2019_clean <- divvy_2019 %>%
  rename(
    ride_id = trip_id,
    started_at = start_time,
    ended_at = end_time,
    start_station_id = from_station_id,
    start_station_name = from_station_name,
    end_station_id = to_station_id,
    end_station_name = to_station_name,
    member_casual = usertype
  ) %>%
  mutate(
    member_casual = case_when(
      member_casual == "Subscriber" ~ "member",
      member_casual == "Customer" ~ "casual"
    ),
    rideable_type = "docked_bike"
  ) %>%
  select(
    ride_id, rideable_type, started_at, ended_at,
    start_station_name, start_station_id,
    end_station_name, end_station_id, member_casual
  )

# Clean 2020 data
divvy_2020_clean <- divvy_2020 %>%
  select(
    ride_id, rideable_type, started_at, ended_at,
    start_station_name, start_station_id,
    end_station_name, end_station_id, member_casual
  )

# Combine
all_trips <- bind_rows(
  divvy_2019_clean %>% mutate(ride_id = as.character(ride_id)),
  divvy_2020_clean %>% mutate(ride_id = as.character(ride_id))
) %>%
  mutate(
    started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
    ended_at   = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S"),
    ride_length_secs = as.numeric(difftime(ended_at, started_at, units = "secs")),
    ride_length = hms::hms(ride_length_secs),
    day_of_week = lubridate::wday(started_at, week_start = 7)
  )

# -------------------------------------------------
# 3. 🔍 Exploratory Analysis & Visualizations
# -------------------------------------------------

# 3.1 Total rides by rider type
all_trips %>%
  group_by(member_casual) %>%
  summarise(total_rides = n()) %>%
  ggplot(aes(x = member_casual, y = total_rides, fill = member_casual)) +
  geom_col() +
  labs(title = "📊 Total Rides by Rider Type",
       x = "Rider Type", y = "Total Rides") +
  theme_minimal()

# 3.2 Average ride length by rider type
all_trips %>%
  group_by(member_casual) %>%
  summarise(avg_ride_length = mean(ride_length_secs, na.rm = TRUE)) %>%
  ggplot(aes(x = member_casual, y = avg_ride_length, fill = member_casual)) +
  geom_col() +
  labs(title = "⏱ Average Ride Length by Rider Type",
       x = "Rider Type", y = "Average Ride Length (seconds)") +
  theme_minimal()

# 3.3 Rides by day of week
all_trips %>%
  group_by(member_casual, day_of_week) %>%
  summarise(num_rides = n(), .groups = "drop") %>%
  ggplot(aes(x = factor(day_of_week, levels = 1:7),
             y = num_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "📅 Rides by Day of Week",
       x = "Day of Week (1=Sun, 7=Sat)", y = "Number of Rides") +
  theme_minimal()

# 3.4 Ride patterns by time of day
all_trips %>%
  mutate(hour = hour(started_at)) %>%
  group_by(member_casual, hour) %>%
  summarise(num_rides = n(), .groups = "drop") %>%
  ggplot(aes(x = hour, y = num_rides, color = member_casual)) +
  geom_line(size = 1.2) +
  labs(title = "🌞 Ride Patterns by Time of Day",
       x = "Hour of Day", y = "Number of Rides") +
  theme_minimal()

# 3.5 Top 10 Start Stations by Rider Type
all_trips %>%
  group_by(member_casual, start_station_name) %>%
  summarise(num_rides = n(), .groups = "drop") %>%
  arrange(desc(num_rides)) %>%
  group_by(member_casual) %>%
  slice_max(num_rides, n = 10) %>%
  ggplot(aes(x = reorder(start_station_name, num_rides), y = num_rides, fill = member_casual)) +
  geom_col() +
  coord_flip() +
  labs(title = "🏙 Top 10 Start Stations by Rider Type",
       x = "Start Station", y = "Number of Rides") +
  theme_minimal()

# -------------------------------------------------
# 4. ✅ Conclusion & Recommendations
# -------------------------------------------------

cat("
## 📊 Findings

1. **Ride Volume**  
   - 🚴 Members take far more rides than casual riders — they form the majority of Cyclistic’s usage.  
   - Casual riders contribute only a small portion of overall trips.  

2. **Ride Duration**  
   - ⏱ Casual riders have much longer average ride lengths (often for leisure or exploration).  
   - Members typically use bikes for shorter, quick trips — likely commuting.  

3. **Distribution of Ride Lengths**  
   - 📈 Most **member rides are short (<15 minutes)**, showing efficiency and utility for commuting.  
   - Casual rides are more spread out, with some lasting significantly longer — highlighting recreational use.  

4. **Ride Patterns by Time of Day**  
   - ⏰ Members have **clear commute peaks** around **7–9 AM** and **4–6 PM**, aligning with work hours.  
   - Casual riders show a steadier distribution throughout the day, with slightly higher activity in the afternoon.  

5. **Rides by Day of Week**  
   - 📅 Members ride more during weekdays, supporting the commuter usage pattern.  
   - Casual riders peak on weekends, confirming their recreational and leisure-based trips.  

6. **Top Start Stations**  
   - 🏙 Members mostly start rides at **busy commuter stations** (e.g., Canal St., Clinton St., Columbus Dr.) near offices and transport hubs.  
   - 🚦 Casual riders often start at **tourist-heavy or recreational stations** (e.g., Streeter Dr., Lake Shore Dr., Millennium Park, Shedd Aquarium).  

---

## ✅ Recommendations

1. **🎯 Targeted Marketing at Tourist & Recreational Spots**  
   - Place promotions for memberships at tourist-heavy stations and weekend hotspots to capture casual riders.  

2. **💡 Highlight Membership Value**  
   - Showcase **cost savings for long rides** and emphasize convenience for frequent usage.  

3. **🏖 Seasonal & Weekend Deals**  
   - Launch **short-term weekend or seasonal passes** to appeal to casual riders who mainly ride for leisure.  

4. **📢 Commuter-Oriented Campaigns**  
   - Focus ads and promotions at transport-linked stations to reinforce the benefits of memberships for daily commuting.  

5. **🔗 Digital Nudges & Personalization**  
   - Send **in-app reminders, discounts, or trial memberships** when casual riders complete multiple long rides.  

6. **🌐 Time-of-Day Promotions**  
   - Introduce **off-peak offers** to attract casual riders during afternoons and weekends when they are most active.  

---

## 👉 Business Impact  

By aligning marketing strategies with rider behavior:  
- **Members’ commuter habits** can be reinforced, keeping retention strong.  
- **Casual riders’ leisure patterns** can be targeted with tailored offers, converting them into paying members.  
- This dual approach will increase **predictable recurring revenue**, improve **bike utilization across times of day**, and strengthen **Cyclistic’s customer loyalty base**.  


")
