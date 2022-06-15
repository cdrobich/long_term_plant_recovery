
# Library -----------------------------------------------------------------

library(tidyverse)
library(plotrix) # SER function
library(ggsci) # colour palettes
library(patchwork)

# Data --------------------------------------------------------------------

univariate <- read.csv("Data/plant_alpha_diversity.csv")
univariate$sample_year <- as.factor(univariate$sample_year)
str(univariate)

beta <- read.csv("Data/beta_diversity_table.csv")
beta <- janitor::clean_names(beta)

univariate_sum <- read.csv("Data/univariate_summary.csv")


# Colour Scheme -----------------------------------------------------------

colours = c("Control" = "#1b7837",
           "Treatment" = "#762a83")


# Alpha Diversity Violin Figures -----------------------------------------------------------------

colnames(univariate)

ggplot(univariate, aes(fill = treatment,
                       y = richness,
                       x = sample_year)) +
  geom_violin(trim = FALSE,
              position = position_dodge(0.8)) +
  stat_summary(
    aes(shape = treatment),
    fun.data = "mean_sdl", fun.args = list(mult = 1), # average and standard deviation
    geom = "pointrange", size = 1,
    position = position_dodge(0.8)) +
  theme_minimal() +
  ylab("Species Richness") +
  xlab(" ") +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))







# Alpha Diversity Line Plots ----------------------------------------------
colnames(univariate_sum)

richness_lineplot <- univariate_sum %>% 
  ggplot(aes(x = sample_year, y = richness_Mean,
             colour = treatment,
             group = treatment)) +
  geom_errorbar(aes(ymin = richness_Mean - richness_SE,
                    ymax = richness_Mean + richness_SE),
                width = 0.1,
                lwd = 1.5) +
  geom_line(lwd = 1.5) +
  geom_point(size = 5) +
  ylim(0, 6) +
  theme_light() +
  scale_colour_jama() +
  ylab("Species Richness") +
  xlab(" ") +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))



shannon_lineplot <- univariate_sum %>% 
  ggplot(aes(x = sample_year, y = shannon_Mean,
             colour = treatment,
             group = treatment)) +
  geom_errorbar(aes(ymin = shannon_Mean - shannon_SE,
                    ymax = shannon_Mean + shannon_SE),
                width = 0.1,
                lwd = 1.5) +
  geom_line(lwd = 1.5) +
  geom_point(size = 5) +
  ylim(0, 1) +
  theme_light() +
  scale_colour_jama() +
  ylab("Shannon-Weiner Diversity") +
  xlab(" ") +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))


pielou_lineplot <- univariate_sum %>% 
  ggplot(aes(x = sample_year, y = pielou_Mean,
             colour = treatment,
             group = treatment)) +
  geom_errorbar(aes(ymin = pielou_Mean - pielou_SE,
                    ymax = pielou_Mean + pielou_SE),
                width = 0.1,
                lwd = 1.5) +
  geom_line(lwd = 1.5) +
  geom_point(size = 5) +
  ylim(0, 1) +
  theme_light() +
  scale_colour_jama() +
  ylab("Pielou's Evenness") +
  xlab(" ") +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))


abundance_lineplot <- univariate_sum %>% 
  ggplot(aes(x = sample_year, y = abundance_Mean,
             colour = treatment,
             group = treatment)) +
  geom_errorbar(aes(ymin = abundance_Mean - abundance_SE,
                    ymax = abundance_Mean + abundance_SE),
                width = 0.1,
                lwd = 1.5) +
  geom_line(lwd = 1.5) +
  geom_point(size = 5) +
  theme_light() +
  ylim(10, 100) +
  scale_colour_jama() +
  ylab("Total % Living Cover") +
  xlab(" ") +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))


# Beta Diversity plots ----------------------------------------------------

colnames(beta)
str(beta)
beta$Year <- as.factor(beta$Year)

total_beta <- beta %>% 
  ggplot(aes(x = year, y = sum,
             colour = treatment,
             group = treatment)) +
  geom_line(lwd = 1.5) +
  geom_point(size = 5) +
  theme_light() +
  scale_colour_jama() +
  ylim(0.85, 1) +
  ylab("Total Beta Diversity") +
  xlab(" ") +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = "none")


beta %>% 
  ggplot(aes(x = Year, y = Turnover,
             colour = Treatment,
             group = Treatment)) +
  geom_line(lwd = 1.5) +
  geom_point(size = 5) +
  theme_minimal() +
  scale_colour_jama() +
  ylab("Turnover") +
  xlab(" ") +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = "right")

beta %>% 
  ggplot(aes(x = Year, y = Nestedness,
             colour = Treatment,
             group = Treatment)) +
  geom_line(lwd = 1.5) +
  geom_point(size = 5) +
  theme_minimal() +
  scale_colour_jama() +
  ylab("Nestedness") +
  xlab(" ") +
  theme(text = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.position = "right")


# Putting Panels together -------------------------------------------------

combined <- abundance_lineplot + richness_lineplot +
  shannon_lineplot + pielou_lineplot + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom") 

combined + total_beta + lake_erie_plot +
  plot_annotation(tag_levels = 'A') &
  theme(plot.tag = element_text(size = 14))
