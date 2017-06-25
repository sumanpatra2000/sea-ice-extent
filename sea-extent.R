setwd("C:/Users/Kanishka/sea-ice-extent")
library(tidyverse)
library(lubridate)
library(extrafont)
library(kani)
library(xts)
library(timekit)
library(forecast)

sea_ice <- read.csv("seaice.csv") %>% select(-Source.Data)

sea_ice <- sea_ice %>%
  mutate(date = ymd(paste(Year, Month, Day, sep = "-"))) %>%
  filter(Year >= 2000)

north <- sea_ice %>% filter(hemisphere == "north")
  
northts <- xts(north %>% select(Extent), order.by = north$date, frequency = 12)
# autoplot(decompose(northts))

north %>%
  ggplot(aes(date, Extent, color = as.factor(Month))) + 
  geom_line() + 
  theme_kani() + 
  scale_x_date(limits = c(as.Date("2000", format = "%Y"), as.Date("2018", format = "%Y")))

year_fit <- lm(Extent ~ factor(Year), data = sea_ice)
sea_ice$annual_avg <- fitted(year_fit)
sea_ts <- ts(sea_ice$Extent, start = 2000, frequency = 365.25)
sea_ice$weekly_ma <- stats::filter(sea_ts, filter = rep(1,7)/7)

qplot(Year, annual_avg, data = sea_ice %>% filter(hemisphere == "north"), geom = "line")

sea_ice %>%
  # filter(Day == 25 & Month == 12) %>%
  filter(hemisphere == "north") %>%
  ggplot(aes(date, Extent, color = hemisphere, group = hemisphere)) + 
  geom_line(size = 1.4) + 
  geom_line(aes(y = annual_avg), size = 1.4, color = "#30a9de") + 
  # geom_line(aes(y = weekly_ma), size = 1.4, color = "red") + 
  theme_kani() + 
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  # scale_y_continuous(breaks = seq(5, 15, by = 2.5), limits = c(5, 15))+
  labs(y = "Extent(Million sq. km)")


