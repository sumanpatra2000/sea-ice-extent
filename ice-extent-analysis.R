setwd("C:/Users/Kanishka/sea-ice-extent")
setwd("sea-ice-extent/")
library(tidyverse)
library(lubridate)
library(extrafont)
library(kani)
library(directlabels)
library(gganimate)
library(scales)
library(tweenr)

size_change <- function(x) {
  ifelse(x > 2013, 1.4, 0.3)
}

sea_ice <- read.csv("seaice.csv") %>% select(-Source.Data)

sea_ice <- sea_ice %>% 
  # filter(hemisphere == "south") %>%
  inner_join(data.frame(month.abb) %>%
               mutate(Month = row_number()))
sea_ice$month.abb <- factor(sea_ice$month.abb, levels = month.abb)

twenty_years <- sea_ice %>%
  filter(Year <= 1999 & Year > 1978)

rest <- sea_ice %>%
  filter(Year > 1999)

avg <- mean(twenty_years$Extent)
med <- median(twenty_years$Extent)

north_previous <- sea_ice %>%
  filter(Year <= 1999 & hemisphere == "north" & Year > 1978)

north_rest <- sea_ice %>%
  filter(Year > 1999 & hemisphere == "north")

south_previous <- sea_ice %>%
  filter(Year <= 1999 & hemisphere == "south" & Year > 1978)

south_rest <- sea_ice %>%
  filter(Year > 1999 & hemisphere == "south")

avg_north <- mean(north_previous$Extent)
avg_south <- mean(south_previous$Extent)
med_north <- median(north_previous$Extent)
med_south <- median(south_previous$Extent)

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

p <- year_extents %>%
  ggplot(aes(month.abb, avg_diff, color = group_code)) + 
  geom_line(aes(group = Year, size = size_change(Year))) + 
  scale_color_manual(values = c("black", "grey", "#f17f42")) + 
  geom_hline(yintercept = 0) + 
  geom_dl(aes(label = label_code), method = list(dl.trans(x = x - 0.1, y = y + 0.47), "last.points", fontfamily = "Roboto Condensed", fontface = "bold")) + 
  # geom_text(aes(label = label_code)) + 
  scale_size_identity() + 
  scale_y_continuous(breaks = seq(-4.5, 2.5, by = 0.5)) + 
  theme_kani() + 
  theme(legend.position = "None", plot.caption = element_text(size = 12)) +
  labs(title = "Extent of ice in seas from 2000-2017",
       subtitle = "As average difference from 1979-1999 average extent",
       x = "Month", y = "Average Difference (in million sq. km)",
       caption = "By Kanishka Misra\nSource: Kaggle and NSIDC")

p

ggsave("average difference.png", p, height = 8, width = 8)

## Hemisphere-wise analysis
hemi_year_extents <- rest %>%
  group_by(Year, month.abb, Month,  hemisphere) %>%
  summarise(avg_extent = mean(Extent),
            med_extent = median(Extent))

hemi_year_extents$month.abb <- factor(hemi_year_extents$month.abb, levels = month.abb)

hemi_year_extents <- hemi_year_extents %>%
  arrange(Year) %>%
  mutate(avg_diff = ifelse(hemisphere == "north", avg_extent - avg_north, avg_extent - avg_south),
         med_diff = ifelse(hemisphere == "north", med_extent - med_north, med_extent - med_south),
         group_code = ifelse(Year %in% c(2014, 2015, 2016), "one",
                             ifelse(Year == 2017, "two", "three")),
         label_code = ifelse(Year %in% c(2014, 2015, 2016, 2017), as.character(Year),""),
         size_code = ifelse(Year %in% c(2014, 2015, 2016, 2017), 1,0.5))


north_p <- hemi_year_extents %>%
  filter(hemisphere == "north") %>%
  ggplot(aes(month.abb, avg_diff, color = group_code)) + 
  geom_line(aes(group = Year, size = size_change(Year))) + 
  scale_color_manual(values = c("black", "grey", "#f17f42")) + 
  geom_hline(yintercept = 0) + 
  geom_dl(aes(label = label_code), method = list(dl.trans(x = x + 0.1, y = y + 0.15), "last.points", fontfamily = "Roboto Condensed", fontface = "bold")) + 
  # geom_text(aes(label = label_code)) + 
  scale_size_identity() + 
  scale_y_continuous(breaks = seq(-9, 4, by = 1)) + 
  theme_kani() + 
  theme(legend.position = "None", plot.caption = element_text(size = 12)) +
  labs(title = "Extent of ice in seas in the north from 2000-2017",
       subtitle = "As average difference from 1979-1999 average extent",
       x = "Month", y = "Average Difference (in million sq. km)",
       caption = "By Kanishka Misra\nSource: Kaggle and NSIDC")
north_p
ggsave("north_avg.png", north_p, height = 9 , width = 9)


south_p <- hemi_year_extents %>%
  filter(hemisphere == "south") %>%
  ggplot(aes(month.abb, avg_diff, color = group_code)) + 
  geom_line(aes(group = Year, size = size_change(Year))) + 
  scale_color_manual(values = c("black", "grey", "#f17f42")) + 
  geom_hline(yintercept = 0) + 
  geom_dl(aes(label = label_code), method = list(dl.trans(x = x + 0.1, y = y + 0.15), "last.points", fontfamily = "Roboto Condensed", fontface = "bold")) + 
  # geom_text(aes(label = label_code)) + 
  scale_size_identity() + 
  scale_y_continuous(breaks = seq(-11, 8, by = 1)) + 
  theme_kani() + 
  theme(legend.position = "None", plot.caption = element_text(size = 12)) +
  labs(title = "Extent of ice in seas in the south from 2000-2017",
       subtitle = "As average difference from 1978-1999 average extent",
       x = "Month", y = "Average Difference (in million sq. km)",
       caption = "By Kanishka Misra\nSource: Kaggle and NSIDC")

south_p
ggsave("south_avg.png", south_p, height = 9, width = 9)

#### Month analysis of all years.

all_years <- sea_ice %>% 
  # filter(hemisphere == "south") %>%
  inner_join(data.frame(month.abb) %>%
               mutate(Month = row_number())) %>%
  group_by(Year, month.abb) %>%
  summarise(avg = mean(Extent),
            med = median(Extent))

all_years$month.abb <- factor(all_years$month.abb, levels = month.abb)

all_years <- all_years %>%
  arrange(Year, month.abb)

all_years<- all_years %>%
  # filter(hemisphere == "north") %>%
  # select(-hemisphere) %>%
  mutate(rank_avg = rank(-avg),
         rank_med = rank(-med))

all_years$rank_avg <- factor(all_years$rank_avg, levels = seq(12,1))
all_years$rank_med <- factor(all_years$rank_med, levels = seq(12,1))

ranks <- all_years %>%
  # filter(Year > 1999 & Year < 2017) %>%
  filter(Year > 1978 & Year < 2017) %>%
  ggplot(aes(Year, rank_avg, color = month.abb)) +
  geom_line(aes(group = month.abb), size = 1) + 
  geom_dl(aes(label = month.abb), method = list(dl.trans(x = x + 0.1, y = y + 0.25), "last.points", fontfamily = "Roboto Condensed", fontface = "bold")) +
  theme_kani() + 
  # scale_x_continuous(breaks = seq(2000, 2016, by = 2)) + 
  scale_x_continuous(breaks = seq(1979, 2016, by = 4), limits = c(1979, 2016)) + 
  theme(legend.position = "none") + 
  labs(title = "Months with the most ice-covered seas",
       subtitle = "ranked from 2000 to 2016",
       caption = "by Kanishka Misra\nSource: Kaggle and NSIDC")
ranks 
ggsave( "ranks.png", ranks, height = 8, width = 8)
year_extents <- year_extents %>%
  mutate(rank_avg = rank(avg_diff), rank_med = rank(med_diff))


all_years %>%
  ggplot(aes(avg)) + 
  geom_density(aes(group = month.abb, fill = month.abb, color = month.abb), alpha = 0.6) + 
  facet_wrap(~month.abb) + 
  theme_kani()


orange <- "#f17f42"

p_ani <- sea_ice %>%
  mutate(d = ymd(paste(Year,"-",Month,"-",Day, sep = ""))) %>%
  filter(Year == 2016) %>%
  
  ggplot(aes(d, Extent, frame = Month, group = hemisphere)) +
  geom_path(aes(cumulative = T, color = hemisphere), size = 1) + 
  # facet_wrap(~Year, scales = "free_x") + 
  theme_kani() + 
  # scale_color_manual(values = c("#30a9de" , "#e53a40")) + 
  labs(title = "Ice extent in Sea in 2016", x = "Month", y = "Extent (in million sq. km)")
p_ani  
# gganimate(p_ani, interval = 0.1, filename = "animate1.gif")  
gganimate(p_ani, interval = 0.3, title_frame = F, filename = "animate3.gif")  

north_p <- hemi_year_extents %>%
  filter(hemisphere == "north") %>%
  ggplot(aes(month.abb, avg_diff, color = group_code, frame = Month)) + 
  geom_line(aes(cumulative = T, group = Year, size = size_change(Year))) + 
  scale_color_manual(values = c("black", "grey", "#f17f42")) + 
  geom_hline(yintercept = 0) + 
  geom_dl(aes(label = label_code), method = list(dl.trans(x = x + 0.1, y = y + 0.15), "last.points", fontfamily = "Roboto Condensed", fontface = "bold")) + 
  # geom_text(aes(label = label_code)) + 
  scale_size_identity() + 
  scale_y_continuous(breaks = seq(-9, 4, by = 1)) + 
  theme_kani() + 
  theme(legend.position = "None", plot.caption = element_text(size = 12)) +
  labs(title = "Extent of ice in seas in the north from 2000-2017",
       subtitle = "As average difference from 1979-1999 average extent",
       x = "Month", y = "Average Difference (in million sq. km)",
       caption = "By Kanishka Misra\nSource: Kaggle and NSIDC")
north_p  
gganimate(north_p, interval = 0.6, filename = "animate2.gif")

ranks_tween <- all_years %>% 
  filter(Year > 1978 & Year < 2017) %>%
  ungroup() %>%
  mutate(rank_avg = as.numeric(rank_avg), Year = as.numeric(Year)) %>%
  select(Year, month.abb, rank_avg) %>%
  split(.$Year) %>%
  tweenr::tween_states(tweenlength = 5, statelength = 0, ease = "cubic-in-out", nframes = 300)

ranks_tween$rank_avg <- factor(ranks_tween$rank_avg, levels = seq(12,1))
?tween_states
ranks_ani <- ranks_tween %>%
  ggplot(aes(Year, rank_avg, color = month.abb, frame = .frame)) +
  geom_path(aes(cumulative = T, group = month.abb), size = 1) + 
  geom_dl(aes(label = month.abb), method = list(dl.trans(x = x + 0.1, y = y + 0.25), "last.points", fontfamily = "Roboto Condensed", fontface = "bold")) +
  theme_kani() + 
  # scale_x_continuous(breaks = seq(2000, 2016, by = 2)) + 
  scale_x_continuous(breaks = seq(1979, 2016, by = 4), limits = c(1979, 2016)) + 
  scale_y_continuous(breaks = seq(1,12, by = 1), labels = as.character(seq(12,1)))+
  theme(legend.position = "none") + 
  labs(title = "Months with the most ice-covered seas",
       subtitle = "ranked from 1979 to 2016",
       caption = "by Kanishka Misra\nSource: Kaggle and NSIDC")
ranks_ani
# gganimate(ranks_ani, interval = 0.2, title_frame = F, "animationwoah.gif")
gganimate(ranks_ani, interval = 0.1, title_frame = F, ani.width = 600, filename = "animationyesss.gif")

sea_ice %>%
  filter(Year == 2016)
