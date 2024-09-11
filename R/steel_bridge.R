# load the data
load("Data/Hourly_Sub_Location_Id_22169552.RData")

library(sf)
library(leaflet)
library(dplyr)
library(tmap)
library(ggplot2)

counters <- st_read("Data/Site_Location_Info/Site_Location_Info.shp")
counters %>% count(City)
counters %>% count(Usr_T_D)
  
counters_pdx <- counters |> filter(City=="Portland") 
counters_pdx
plot(counters_pdx["Name"])
?plot
plot.sf

leaflet() %>%
  addTiles() %>%
  setView(lng = -122.5, lat = 45.5, zoom = 9) %>% 

# https://stackoverflow.com/questions/41940403/popup-on-a-shape-using-tmap  
tm_counters_pdx <- tm_shape(counters_pdx) + tm_dots(id="Name", popup.vars=c("name"="Name"))
tm_counters_pdx <- tm_shape(counters_pdx) + tm_dots(id="Name")

tmap_leaflet(tm_counters_pdx)

steel_bridge <- Hourly_Sub_Location_Id.. %>% ungroup() %>% filter(Device_Name == "RIVER WALK")
steel_bridge
sb1 <- steel_bridge %>% select(Device_Name, Date, Hour, Direction, Counts, 
                               Month, Year)
sb1
saveRDS(sb1, "Data/steel_bridge_counts_jan2013_may2024.RDS")

steel_bridge <- readRDS("Data/steel_bridge_counts_jan2013_may2024.RDS")
steel_bridge 
summary(steel_bridge)

steel_bridge %>% 
  count(Month)

steel_bridge <- steel_bridge %>% 
  mutate(Month = factor(Month, levels = month.name)) 

steel_bridge %>% 
  count(Month)

steel_bridge %>% arrange(desc(Date))
steel_bridge %>% filter(!is.na(Counts)) %>% 
                          count(Year)
steel_bridge %>% group_by(Year, Month, Date) %>% 
  summarize(daily = sum(Counts, na.rm=T)) %>% 
  summarize(max(daily))
steel_bridge %>% group_by(Year, Date) %>% 
  summarize(daily = sum(Counts, na.rm=T)) %>% 
  arrange(desc(daily)) %>% print(n=20)
steel_bridge %>% group_by(Year, Month, Date) %>% 
  summarize(daily = sum(Counts, na.rm=T)) %>% 
  summarize(max(daily))

saveRDS(steel_bridge, "Data/steel_bridge_counts_jan2013_may2024.RDS")
steel_bridge <- readRDS("Data/steel_bridge_counts_jan2013_may2024.RDS")

daily <- steel_bridge %>% group_by(Date) %>% 
  summarize(daily_tot = sum(Counts))
summary(daily)

monthly <- daily %>% group_by(month=format(Date, "%Y-%m")) %>% 
  summarize(avg_daily = mean(daily_tot, na.rm=T))
monthly

p <- monthly %>% 
  ggplot(aes(x = as.Date(month, "%Y-%m"), y = avg_daily, group = 1)) + 
  geom_line()
p

table(daily$Month, daily$Year)

daily <- steel_bridge %>% group_by(Year, Month, Date) %>% 
  summarize(daily = sum(Counts, na.rm=T))
table(daily$Month, daily$Year)

daily_2018 <- daily[daily$Year==2018,]
summary(daily_2018)

plot(daily_2018$Date, daily_2018$daily)

daily %>% filter(Year==2018) %>% 
  ggplot(aes(x=Date, y=daily)) +
  geom_point() + 
  facet_wrap(~Year)

daily %>% 
  ggplot(aes(x=Date, y=daily)) +
  geom_point() + 
  facet_wrap(~Year, scales="free")

boxplot(daily$daily ~ daily$Year)
daily %>% ggplot(aes(x=Year, y=daily)) + 
  geom_boxplot()

daily %>% filter(Year >= 2019) %>% 
  ggplot(aes(x=Year, y=daily)) + 
  geom_boxplot()

month.name[1:5]
daily %>% filter(Year >= 2019 & Month %in% month.name[1:5]) %>% 
  ggplot(aes(x=Year, y=daily)) + 
  geom_boxplot()

daily %>% filter(Year >= 2019 & Month %in% month.name[1:5]) %>% 
  group_by(Year) %>% 
  summarize(ADB = mean(daily))

daily %>% filter(Year >= 2019) %>% 
  ggplot(aes(x=Date, y=daily)) + 
  geom_line() + 
  geom_vline(xintercept = as.Date("2020-03-23"), col="red")

boxplot(daily %>% filter(!Year %in% c(2013, 2017:2018)) %>% pull(daily) ~ 
          daily %>% filter(!Year %in% c(2013, 2017:2018)) %>% pull(Year))
boxplot(daily[!daily$Year %in% c(2013, 2017:2018), "daily"] ~ 
          daily[!daily$Year %in% c(2013, 2017:2018), "Year"])
daily_valid <- daily[!daily$Year %in% c(2013, 2017, 2018), ]
daily_valid
boxplot(daily_valid$daily ~ daily_valid$Year, 
             col=colorRampPalette(c("blue", "green"))(11))
daily %>% arrange(desc(Date)) 

valid_months <- c("January", "February", "March", "April", "May")
daily_valid_jan_may <- daily_valid %>% filter(Month %in% valid_months)
unique(daily_valid_jan_may$Year)
unique(daily_valid_jan_may$Year)

table(daily_valid_jan_may$Month, daily_valid_jan_may$Year)
boxplot(daily_valid_jan_may$daily ~ daily_valid_jan_may$Year, 
        col=c(rep("white", 8), "yellow"))
boxplot(daily_valid$daily ~ daily_valid$Year, 
        col=c(rep("white", 8), "yellow"))

?tm_dots
?format

x <- c(a = 1, b = 2)
x
is.vector(x)
as.vector(x)
all.equal(x, as.vector(x)) ## FALSE

