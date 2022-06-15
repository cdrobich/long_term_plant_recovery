
# Library -----------------------------------------------------------------

library(tidyverse)
library(plotrix) # SER function

# Data --------------------------------------------------------------------

univariate <- read.csv("Data/plant_alpha_diversity.csv")
univariate$sample_year <- as.factor(univariate$sample_year)
str(univariate)

beta <- read.csv("Data/beta_diversity_table.csv")


# Summary statistics ------------------------------------------------------

univariate_summary <- univariate %>% 
  group_by(treatment, sample_year) %>% 
  summarise(across(
    .cols = where(is.numeric),
    .fns = list(Mean = mean, SD = sd, SE = std.error), na.rm = TRUE,
    .names = "{col}_{fn}"
  ))


write.csv(univariate_summary, "Data/univariate_summary.csv",
          row.names = FALSE)

# Alpha Diversity Figures -----------------------------------------------------------------
colnames(univariate)

ggplot(univariate, aes(fill = treatment, y = richness, x = sample_year)) +
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
