library(tidyverse)
library(lubridate)
library(extrafont)
library(kani)
library(directlabels)
library(gganimate)
library(scales)
library(tweenr)

sea_ice <- read.csv("https://raw.githubusercontent.com/kanishkamisra/sea-ice-extent/master/seaice.csv") %>% select(-Source.Data)

sea_ice <- sea_ice %>% 
  inner_join(data.frame(month.abb) %>%
               mutate(Month = row_number()))

sea_ice$month.abb <- factor(sea_ice$month.abb, levels = month.abb)

all_years <- sea_ice %>% 
  # filter(hemisphere == "south") %>%
  inner_join(data.frame(month.abb) %>%
               mutate(Month = row_number())) %>%
  group_by(Year, month.abb) %>%
  summarise(avg = mean(Extent),
            med = median(Extent))

all_years <- all_years %>%
  arrange(Year, month.abb) %>%
  mutate(rank_avg = rank(avg),
         rank_med = rank(med))

all_years$month.abb <- factor(all_years$month.abb, levels = month.abb)

ranks_tween <- all_years %>% 
  filter(Year > 1978 & Year < 2017) %>%
  ungroup() %>%
  mutate(rank_avg = as.numeric(rank_avg), Year = as.numeric(Year)) %>%
  select(Year, month.abb, rank_avg) %>%
  split(.$Year) %>%
  tweenr::tween_states(tweenlength = 5, statelength = 0, ease = "cubic-in-out", nframes = 300)

ranks_tween$month.abb <- factor(ranks_tween$month.abb, levels = month.abb)

ranks_ani <- ranks_tween %>%
  ggplot(aes(Year, rank_avg, color = month.abb, frame = .frame)) +
  geom_path(aes(cumulative = T, group = month.abb), size = 1) + 
  geom_dl(aes(label = month.abb), method = list(dl.trans(x = x + 0.1, y = y + 0.25), "last.points", fontfamily = "Roboto Condensed", fontface = "bold")) +
  theme_kani() + 
  scale_x_continuous(breaks = seq(1979, 2016, by = 4), limits = c(1979, 2016)) + 
  scale_y_continuous(breaks = seq(1,12, by = 1), labels = as.character(seq(12,1)))+
  theme(legend.position = "none") + 
  labs(title = "Months with the most ice-covered seas",
       subtitle = "ranked from 1979 to 2016",
       caption = "by Kanishka Misra\nSource: Kaggle and NSIDC")

ranks_ani

gganimate(ranks_ani, interval = 0.1, title_frame = F, ani.width = 600, filename = "animation-months.gif")