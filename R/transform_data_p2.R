#### Transforming Data Part 2 ####
library(readxl)
library(dplyr)
library(ggplot2)

# Distinct & Select (reducing data rows or columns) ----
df <- read_excel("Data/icebreaker_answers.xlsx")
df <- df %>% bind_rows(slice_tail(df))
tail(df)

df %>% group_by_all() %>% 
  mutate(duplicated = n() > 1) %>% 
  filter(duplicated)

df <- df |> distinct()
tail(df)

df_travel <- df %>% 
  select(travel_mode:travel_distance)
df_travel

df_travel <- df %>% 
  select(starts_with("travel_"))
df_travel

df_travel <- df %>% 
  select(-serial_comma)
df_travel

# mutate and rename (modifying data frame in place) ----
df <- df %>% mutate(travel_speed = travel_distance / 
                      travel_time * 60)  
df

df <- df %>% rename(travel_mph = travel_speed)  
df

df <- df %>% mutate(long_trip = if_else(travel_distance > 20, 
                                        TRUE, FALSE))
df %>% select(-serial_comma)
boxplot(df$travel_mph ~ df$long_trip)

df <- df %>% mutate(slow_trip = case_when(
  travel_mode == "bike" & travel_mph < 12 ~ 1,
  travel_mode == "car" & travel_mph < 25 ~ 1,
  travel_mode == "bus" & travel_mph < 15 ~ 1,
  travel_mode == "light rail" & travel_mph < 20 ~ 1,
  .default = 0  # All FALSE or NA 
))
df %>% filter(slow_trip == 0)

df %>% select(-serial_comma, -long_trip) %>% 
  arrange(travel_mode, desc(travel_mph)) %>% 
  print(n=30)

