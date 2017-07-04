setwd("C:/Users/Kanishka/sea-ice-extent/")

library(tidyverse)
library(extrafont)
library(kani)
library(directlabels)
library(timekit)

size_change <- function(x) {
  ifelse(x > 2013, 1.4, 0.3)
}

seaice <- read.csv("seaice.csv") %>% select(-Source.Data)
seaice <- seaice %>% select(-Missing)
seaice <- seaice %>% 
  inner_join(data.frame(month.abb) %>%
               mutate(Month = row_number()))

seaice$month.abb <- factor(seaice$month.abb, levels = month.abb)

# all years average and median difference from 1979 to 1999

previous <- seaice %>%
  filter(Year >= 1979 & Year <= 1999) %>%
  group_by(month.abb) %>%
  summarise(avg_previous = mean(Extent), med_previous = median(Extent))

rest <- seaice %>%
  filter(Year > 1999) %>%
  group_by(Year, month.abb) %>%
  summarise(avg = mean(Extent), med = median(Extent))

rest %>%
  inner_join(previous) %>%
  mutate(avg_diff = avg - avg_previous,
         med_diff = med - med_previous,
         group_code = ifelse(Year %in% c(2014, 2015, 2016), "one",
                             ifelse(Year == 2017, "two", "three")),
         label_code = ifelse(Year %in% c(2014, 2015, 2016, 2017), as.character(Year),""),
         size_code = ifelse(Year %in% c(2014, 2015, 2016, 2017), 1,0.5)) %>%
  ggplot(aes(month.abb, avg_diff, color = group_code, group = Year)) + 
  geom_line(aes(group = Year, size = size_change(Year))) + 
  scale_color_manual(values = c("black", "grey", "#f17f42")) + 
  geom_hline(yintercept = 0) + 
  geom_dl(aes(label = label_code), method = list(dl.trans(x = x - 0.1, y = y + 0.47), "last.points", fontfamily = "Roboto Condensed", fontface = "bold")) + 
  # geom_text(aes(label = label_code)) + 
  scale_size_identity() +
  theme_kani() +
  theme(legend.position = "None",
        plot.caption = element_text(size = 12),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) +
  labs(title = "Extent of ice in seas from 2000-2017",
       subtitle = "As monthly anomalies relative to 1979-1999 average extent",
       x = "Month", y = "Average monthly difference (in million sq. km)",
       caption = "By Kanishka Misra\nSource: Kaggle and NSIDC")

ggsave("avg anomalies.png", height = 8)

# Hemisphere facetting I guess.

previous_hemi <- seaice %>%
  filter(Year >= 1979 & Year <= 1999) %>%
  group_by(month.abb, hemisphere) %>%
  summarise(avg_previous = mean(Extent), med_previous = median(Extent))

rest_hemi <- seaice %>%
  filter(Year > 1999) %>%
  group_by(Year, month.abb, hemisphere) %>%
  summarise(avg = mean(Extent), med = median(Extent))

rest_hemi %>%
  inner_join(previous_hemi) %>%
  mutate(avg_diff = avg - avg_previous,
         med_diff = med - med_previous,
         group_code = ifelse(Year %in% c(2014, 2015, 2016), "one",
                             ifelse(Year == 2017, "two", "three")),
         label_code = ifelse(Year %in% c(2014, 2015, 2016, 2017), as.character(Year),"")) %>%
  ggplot(aes(month.abb, avg_diff, color = group_code, group = Year)) + 
  geom_line(aes(group = Year, size = size_change(Year))) + 
  scale_color_manual(values = c("black", "grey", "#f17f42")) + 
  geom_hline(yintercept = 0) + 
  geom_dl(aes(label = label_code), method = list(dl.trans( y = y + 0.4), "last.points", fontfamily = "Roboto Condensed", fontface = "bold")) +
  facet_wrap(~hemisphere) +
  scale_y_continuous(breaks = seq(-3.5, 2, by = 0.5)) + 
  scale_size_identity() + 
  theme_kani() +
  theme(legend.position = "None",
        plot.caption = element_text(size = 12),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        strip.text.x = element_text(size = 14)) + 
  labs(title = "The North is on the rise in recent years, while the South..",
       subtitle = "Monthly anomalies in sea-ice extents for 2000-17 relative to 1979-99 average extent",
       x = "Month", y = "Average monthly difference (in million sq. km)",
       caption = "By Kanishka Misra\nSource: Kaggle and NSIDC")
ggsave("hemitemp.png", height = 8, width = 15)

## Dive deep into north - Anomalous behavior..

north <- seaice %>%
  filter(hemisphere == "north")

north %>%
  # mutate(date = lubridate::ymd(paste(Year, Month, Day, sep = "-"))) %>%
  group_by(Year, month.abb) %>%
  summarize(Extent = mean(Extent)) %>%
  ggplot(aes(Year, Extent)) + 
  geom_line(size = 1) + 
  facet_wrap(~month.abb) +
  theme_kani()
  
north %>%
  filter(Year >= 2017) %>%
  mutate(date = lubridate::ymd(paste(Year, Month, Day, sep = "-"))) %>%
  ggplot(aes(date, Extent)) +
  geom_line()

north %>%
  filter(Year >= 2016) %>%
  mutate(date = lubridate::ymd(paste(Year, Month, Day, sep = "-"))) %>%
  ggplot(aes(date, Extent)) +
  geom_line(aes(group = Year, color = factor(Year)))
