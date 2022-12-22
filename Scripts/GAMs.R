library(tidyverse)
library(mgcv)
library(mgcViz)
library(gridExtra)
citation('mgcv')

#https://noamross.github.io/gams-in-r-course/chapter2

data <- read.csv("Data/plant_alpha_diversity.csv")
str(data)

data$site_id <- as.factor(data$site_id)
data$treatment <- as.factor(data$treatment)

str(data)

data <- data %>% 
  mutate(treatment_int = treatment_code * sample_year,
         abundance_arcsin = asin(sqrt(abundance/100)))

head(data)

treated <- data

# test GAM
# "GCV.Cp" to use GCV for unknown scale parameter 
# Select = TRUE then gam can add an extra penalty 
# to each term so that it can be penalized to zero. 
# This means that the smoothing parameter estimation that is part 
# of fitting can completely remove terms from the model

s1 <- gam(richness ~ s(sample_year, k = 5), select = TRUE,
          data = treated,  method = 'GCV.Cp', random = list(site_id = ~1))

summary(s1)

plot(s1)

plot(treated$richness~predict(s1, type="response"))
abline(0,1)


a1 <- gamm(abundance ~ s(sample_year, k = 5), select = TRUE,
          data = treated, method = 'REML', random = list(site_id = ~1))
a1

summary(a1$gam)
summary(a1$lme)

plot(a1$gam)
plot(a1$lme)

plot(treated$abundance~predict(a1, type="response"))
abline(0,1)



# with interaction as code 
str(data)

########### This is the correct version #############

a3 <- gamm(abundance ~ s(sample_year, k = 5, by = treatment) + treatment, select = TRUE,
           data = data, method = 'REML', random = list(site_id = ~1))
a3

summary(a3$gam)
summary(a3$lme)

par(mfrow = c(1, 2))
plot(a3$gam)
plot(a3$gam$residuals)
plot(a3$gam, 1)
plot(a3$gam, 2)

gam.check(a3$gam)


r3 <- gamm(richness ~ s(sample_year, k = 5, by = treatment) + treatment, select = TRUE,
           data = data, method = 'REML', random = list(site_id = ~1))
r3

summary(r3$gam)
summary(r3$lme)

par(mfrow = c(1, 2))
plot(r3$gam)
plot(r3$gam$residuals)


sh3 <- gamm(shannon ~ s(sample_year, k = 5, by = treatment) + treatment, select = TRUE,
           data = data, method = 'REML', random = list(site_id = ~1))
sh3

summary(sh3$gam)
summary(sh3$lme)

par(mfrow = c(1, 2))
plot(sh3$gam)
plot(sh3$gam$residuals)

pi3 <- gamm(pielou ~ s(sample_year, k = 5, by = treatment) + treatment, select = TRUE,
            data = data, method = 'REML', random = list(site_id = ~1))
pi3


summary(pi3$gam)
summary(pi3$lme)

par(mfrow = c(1, 2))

plot(pi3$gam)


# figures
# https://cran.r-project.org/web/packages/mgcViz/vignettes/mgcviz.html

######### Abundance plots ##############


ab3_vis <- gammV(abundance ~ s(sample_year, k = 5, by = treatment) + treatment,
                 data = data, method = 'REML', random = list(site_id = ~1))
ab3_vis

ab1 <- plot(sm(ab3_vis, 1)) 

ab1_plot <- ab1 +  
  l_ciPoly() +
  l_fitLine(colour = "#087E8B", lwd = 2) + 
  theme_classic() + 
  xlab(" ") +
  xlim(2015.7, 2021) +
  ylab("s(treatment year * control plots)") +
  ylim(-55, 45) +
  geom_vline(xintercept = 2016.5, linetype = "dotted",
             colour = "grey", lwd = 1) +
  annotate("text", x = 2015.7, y = 45, label = "A") +
  annotate("text", x = 2020.5, y = 45, label = "Invaded habitat") +
  ggtitle("Living cover abundance") +
  theme(plot.title = element_text(size = 12))


ab2 <- plot(sm(ab3_vis, 2)) 

ab2_plot <- ab2 + 
  l_ciPoly() +
  l_fitLine(colour = "#087E8B", lwd = 1.5) +
  theme_classic() + 
  xlab(" ") +
  xlim(2015.7, 2021) +
  ylab("s(treatment year * treatment plots)") +
  ylim(-55, 45) +
  geom_vline(xintercept = 2016.5, linetype = "dotted",
             colour = "grey", lwd = 1) +
  annotate("text", x = 2015.7, y = 45, label = "B") +
  annotate("text", x = 2020.5, y = 45, label = "Treated habitat") +
  ggtitle(" ") 

gridPrint(ab1_plot, ab2_plot, ncol = 1)




######### Richness  Plots ###############
ri3_vis <- gammV(richness ~ s(sample_year, k = 5, by = treatment) + treatment,
                 data = data, method = 'REML', random = list(site_id = ~1))
ri3_vis

ri1 <- plot(sm(ri3_vis, 1)) 

ri1_plot <- ri1 + 
  l_ciPoly() +
  l_fitLine(colour = "#087E8B", lwd = 2) + 
  theme_classic() + 
  xlim(2015.7, 2021) +
  xlab(" ") +
  ylim(-2, 2) +
  ylab("s(treatment year * control plots)") +
  geom_vline(xintercept = 2016.5, linetype = "dotted",
             colour = "grey", lwd = 1) +
  annotate("text", x = 2015.7, y = 2, label = "C") +
  annotate("text", x = 2020.5, y = 2, label = "Invaded habitat") +
  ggtitle("Species richness") +
  theme(plot.title = element_text(size = 12))


ri2 <- plot(sm(ri3_vis, 2)) 

ri2_plot <- ri2 + 
  l_ciPoly() +
  l_fitLine(colour = "#087E8B", lwd = 1.5) +
  theme_classic() + 
  xlim(2015.7, 2021) +
  xlab(" ") +
  ylim(-2, 2) +
  ylab("s(treatment year * treatment plots)") +
  geom_vline(xintercept = 2016.5, linetype = "dotted",
             colour = "grey", lwd = 1) +
  annotate("text", x = 2015.7, y = 2, label = "D") +
  annotate("text", x = 2020.5, y = 2, label = "Treated habitat")  +
  ggtitle(" ") 

gridPrint(ri1_plot, ri2_plot, ncol = 1)


gridPrint(ab1_plot, ab2_plot,
          ri1_plot, ri2_plot, ncol = 2)


######### Shannon Weiner ###############
sh3_vis <- gammV(shannon ~ s(sample_year, k = 5, by = treatment) + treatment,
                 data = data, method = 'REML', random = list(site_id = ~1))
sh3_vis

sh1 <- plot(sm(sh3_vis, 1)) 

sh1_plot <- sh1 + 
  l_ciPoly() +
  l_fitLine(colour = "#087E8B", lwd = 2) + 
  theme_classic() + 
  xlim(2015.7, 2021) +
  ylim(-1.3, 1.3) +
  xlab(" ") +
  ylab("s(treatment year * control plots)") +
  geom_vline(xintercept = 2016.5, linetype = "dotted",
             colour = "grey", lwd = 1) +
  annotate("text", x = 2015.7, y = 1.3, label = "E") +
  ggtitle("Shannon Weiner Diversity") +
  theme(plot.title = element_text(size = 12))


sh2 <- plot(sm(sh3_vis, 2)) 

sh2_plot <- sh2 +
  l_ciPoly() +
  l_fitLine(colour = "#087E8B", lwd = 2) + 
  theme_classic() + 
  xlim(2015.7, 2021) +
  ylim(-1.3, 1.3) +
  xlab(" ") +
  ylab("s(treatment year * treatment plots)") +
  geom_vline(xintercept = 2016.5, linetype = "dotted",
             colour = "grey", lwd = 1) +
  annotate("text", x = 2015.7, y = 1.3, label = "F") 

gridPrint(sh1_plot, sh2_plot, ncol = 1)



######### Pielou Plots ###############
pi3_vis <- gammV(pielou ~ s(sample_year, k = 5, by = treatment) + treatment,
            data = data, method = 'REML', random = list(site_id = ~1))
pi3_vis

pi1 <- plot(sm(pi3_vis, 1)) 

pi1_plot <- pi1 + 
  l_ciPoly() +
  l_fitLine(colour = "#087E8B", lwd = 2) + 
  theme_classic() + 
  xlim(2015.7, 2021) +
  xlab(" ") +
  ylab("s(treatment year * control plots)") +
  geom_vline(xintercept = 2016.5, linetype = "dotted",
             colour = "grey", lwd = 1) +
  annotate("text", x = 2015.7, y = 1, label = "G") +
  ggtitle("Pielou's Evenness") +
  theme(plot.title = element_text(size = 12))


pi2 <- plot(sm(pi3_vis, 2)) 

pi2_plot <- pi2 +
  l_ciPoly() +
  l_fitLine(colour = "#087E8B", lwd = 2) + 
  theme_classic() + 
  xlim(2015.7, 2021) +
  ylim(-1, 1) +
  xlab(" ") +
  ylab("s(treatment year * treatment plots)") +
  geom_vline(xintercept = 2016.5, linetype = "dotted",
             colour = "grey", lwd = 1) +
  annotate("text", x = 2015.7, y = 1, label = "H") 

gridPrint(pi1_plot, pi2_plot, ncol = 1)





gridPrint(sh1_plot, sh2_plot,
          pi1_plot, pi2_plot, ncol = 2)

# pixel ratio of 1078 x 1400









########## fucking around stuff  #######

# http://r.qcbs.ca/workshop08/book-en/gam-with-interaction-terms.html
# smoothed term that varies by different levels of treatment

a4 <- gamm(abundance ~ s(sample_year, k = 5, by = treatment_int), select = TRUE,
           data = data, method = 'REML', random = list(site_id = ~1))
a4

summary(a4$gam)
summary(a4$lme)


plot(a4$gam)
plot(a4$gam$residuals)
plot(a4$gam, 3)

a5 <- gam(abundance ~ treatment + s(sample_year, k = 5, by = treatment), select = TRUE,
           data = data, method = 'REML')
a5

summary(a5)

plot(a5)
plot(a5$residuals)
plot(a5, 2)
plot(a5, 3)

par(mfrow = c(1,1))
plot(a5)

### trying polynomial 

data <- data %>% 
  mutate(sample_center = scale(sample_year, center = T),
         sample_sqd = scale(sample_year^2, center = T),
         sample_cubed = scale(sample_year^3, center = T))

unique(data$treatment)

ab_1 <- lmer(abundance ~ sample_center*treatment +
             (1 | site_id), data = data)

ab_1
plot(ab_1)

hist(data$sample_cubed)
hist(data$sample_sqd)
