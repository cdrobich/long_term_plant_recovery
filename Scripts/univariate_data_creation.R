
# Library -----------------------------------------------------------------

library(tidyverse) # for data wrangling
library(vegan) # for univariate metrics
library(betapart) # partitioning beta diversity into components

# Data --------------------------------------------------------------------

plant_data <- read.csv("Data/vegetation_data_sparsity.csv")

colnames(plant_data)

# removed "unknown seedling" from choices since it is likely another species

plant_data <- plant_data %>% 
  select(site_code:zizpalus)

# just the species matrix 

plant_species <- plant_data %>% 
  select(boecylin:zizpalus)

# Diversity Metrics -------------------------------------------------------

# Shannon Weiner
H <- diversity(plant_species)

# Pielou's

J <- H/log(specnumber(plant_species))

# Species Richness

S <- specnumber(plant_species)

# Abundance

Ab <- rowSums(plant_species)



# Create Alpha Diversity Dataframe -------------------------------------------------------

plant_diversity <- plant_data %>% 
  select(site_code:treatment) %>% 
  mutate(shannon = H,
         pielou = J,
         richness = S,
         abundance = Ab)


write.csv(plant_diversity, "Data/plant_alpha_diversity.csv",
          row.names = FALSE)







# Beta Diversity ----------------------------------------------------------
unique(plant_data$treatment)
str(plant_data)


control_2016 <- plant_data %>% 
  filter(treatment == "Control",
         sample_year == 2016) %>% 
  select(boecylin:zizpalus)
  



## convert data to presence absence

control_2016[control_2016 > 0] <- 1


# Create dataframe for BETA diversity -------------------------------------

beta_div <- data.frame(matrix(as.numeric(0), ncol=(3), nrow=(12)))
colnames(beta_div) <- c("Turnover","Nestedness","Sum")
rownames(beta_div) <- c("Control_2016", "Treatment_2016",
                        "Control_2017", "Treatment_2017",
                        "Control_2018", "Treatment_2018",
                        "Control_2019", "Treatment_2019",
                        "Control_2020", "Treatment_2020",
                        "Control_2021", "Treatment_2021")

# Beta Diversity calculations ---------------------------------------------

control_2016_bd <- beta.multi(control_2016, index.family = "sorensen")
beta_div[1,] <- data.frame(matrix(unlist(control_2016_bd), nrow = length(1), byrow = T))





