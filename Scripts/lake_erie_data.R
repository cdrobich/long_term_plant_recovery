
# library -----------------------------------------------------------------

library(tidyverse)
library(plotrix)

# data --------------------------------------------------------------------

lake_erie <- read.csv("Data/Lake_Erie_historical.csv")
lake_erie <- janitor::clean_names(lake_erie)

str(lake_erie)
colnames(lake_erie)
unique(lake_erie$month)


water_averages <- lake_erie %>% 
  group_by(year) %>% 
  summarise(water_mean = mean(erie),
            water_sd = sd(erie),
            water_ser = std.error(erie))

lake_erie_plot <- water_averages %>%
  filter(year >= 2010) %>% 
  ggplot(aes(x = year, y = water_mean)) +
  geom_hline(yintercept = 174.17,
             lwd = 2,
             colour = "#F58300") +
  geom_line(lwd = 1.5,
            colour = "#055E8A") +
  geom_point(size = 2,
             colour = "#044362") +
  theme_light() +
  scale_colour_jama() +
  ylab(expression(atop("Monthly mean Lake Erie water levels", paste("(meters IGLD 1985)")))) +
  xlab(" ") +
  geom_label(label = "Historical Average (m)",
             x = 2018,
             y = 174.12,
             size = 3) +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14)) 
