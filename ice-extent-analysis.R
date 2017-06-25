setwd("C:/Users/Kanishka/sea-ice-extent")
library(tidyverse)
library(lubridate)
library(extrafont)
library(kani)
library(directlabels)
library(cairoDevice)

sea_ice <- read.csv("seaice.csv") %>% select(-Source.Data)

twenty_years <- sea_ice %>%
  filter(Year <= 1999)

rest <- sea_ice %>%
  filter(Year > 1999)

avg <- mean(twenty_years$Extent)
med <- median(twenty_years$Extent)

rest <- rest %>% 
  inner_join(data.frame(month.abb) %>%
      mutate(Month = row_number()))

year_extents <- rest %>%
  group_by(Year, month.abb) %>%
  summarise(avg_extent = mean(Extent),
            med_extent = median(Extent))

year_extents$month.abb <- factor(year_extents$month.abb, levels = month.abb)

year_extents <- year_extents %>%
  arrange(Year)

year_extents <- year_extents %>%
  mutate(avg_diff = avg_extent - avg,
         med_diff = med_extent - avg,
         group_code = ifelse(Year %in% c(2014, 2015, 2016), "one",
                             ifelse(Year == 2017, "two", "three")),
         label_code = ifelse(Year %in% c(2014, 2015, 2016, 2017), as.character(Year),""),
         size_code = ifelse(Year %in% c(2014, 2015, 2016, 2017), 1,0.5))

size_change <- function(x) {
  ifelse(x > 2013, 1.4, 0.3)
}

p <- year_extents %>%
  ggplot(aes(month.abb, med_diff, color = group_code)) + 
  geom_line(aes(group = Year, size = size_change(Year))) + 
  scale_color_manual(values = c("black", "grey", "#f17f42")) + 
  geom_hline(yintercept = 0) + 
  geom_dl(aes(label = label_code), method = list(dl.trans(x = x - 0.1, y = y + 0.47), "last.points", fontfamily = "Roboto Condensed", fontface = "bold")) + 
  # geom_text(aes(label = label_code)) + 
  scale_size_identity() + 
  scale_y_continuous(breaks = seq(-4.5, 2.5, by = 0.5)) + 
  theme_kani() + 
  theme(legend.position = "None") +
  labs(title = "Sea-ice cap extent",
       subtitle = "As median difference from 1978-1999 median",
       x = "Month", y = "Average Difference (in million sq. km)")

p

ggsave("median difference.png", p, height = 8, width = 8)

