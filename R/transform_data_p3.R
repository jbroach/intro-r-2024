#### Transforming Data Part 3 ####

library(readxl)
library(dplyr)
library(ggplot2)

df <- read_excel("Data/icebreaker_answers.xlsx")
df

# summari[s|z]ing data ----
df %>% summarize(avg_dist = mean(travel_distance), 
                 med_dist = median(travel_distance), 
                 sd_dist = sd(travel_distance), 
                 Q3_dist = quantile(travel_distance, 
                                    prob = 0.75),
                 n = n())

df %>% group_by(travel_mode) %>%
  summarize(mean_speed = mean(travel_distance / 
                                travel_time * 60))

df %>% mutate(travel_speed = travel_distance / 
                                  travel_time * 60) %>% 
  group_by(travel_mode) %>%
  summarize(mean_speed = mean(travel_speed))

df %>% mutate(travel_speed = travel_distance / 
                travel_time * 60) %>% 
  group_by(travel_mode, serial_comma) %>%
  summarize(mean_speed = mean(travel_speed))

df %>% group_by(serial_comma) %>%
  summarize(n = n())

df %>% group_by(serial_comma) %>%
  tally()

df %>% count(serial_comma)
df %>% count(serial_comma, sort=T)

# mode splits
df %>% group_by(travel_mode) %>%
  summarize(split = n() / nrow(df) * 100) %>% 
  arrange(desc(split))


