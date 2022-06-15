
# Library -----------------------------------------------------------------

library(tidyverse) # for data wrangling
library(vegan) # for univariate metrics
library(betapart) # partitioning beta diversity into components

# Data --------------------------------------------------------------------

plant_data <- read.csv("Data/vegetation_data_sparsity.csv")
plant_data$sample_year <- as.numeric(plant_data$sample_year)

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
unique(plant_data$sample_year)

##### Control Subset ####

control_2016 <- plant_data %>% 
  filter(treatment == "Control",
         sample_year == 2016) %>% 
  select(boecylin:zizpalus)

control_2016[control_2016 > 0] <- 1 # make it presence absence
  

control_2017 <- plant_data %>% 
  filter(treatment == "Control",
         sample_year == 2017) %>% 
  select(boecylin:zizpalus)

control_2017[control_2017 > 0] <- 1


control_2018 <- plant_data %>% 
  filter(treatment == "Control",
         sample_year == 2018) %>% 
  select(boecylin:zizpalus)

control_2018[control_2018 > 0] <- 1


control_2019 <- plant_data %>% 
  filter(treatment == "Control",
         sample_year == 2019) %>% 
  select(boecylin:zizpalus)

control_2019[control_2019 > 0] <- 1


control_2020 <- plant_data %>% 
  filter(treatment == "Control",
         sample_year == 2020) %>% 
  select(boecylin:zizpalus)

control_2020[control_2020 > 0] <- 1


##### Treatment Subset #######

treatment_2016 <- plant_data %>% 
  filter(treatment == "Treatment",
         sample_year == 2016) %>% 
  select(boecylin:zizpalus)

treatment_2016[treatment_2016 > 0] <- 1


treatment_2017 <- plant_data %>% 
  filter(treatment == "Treatment",
         sample_year == 2017) %>% 
  select(boecylin:zizpalus)

treatment_2017[treatment_2017 > 0] <- 1


treatment_2018 <- plant_data %>% 
  filter(treatment == "Treatment",
         sample_year == 2018) %>% 
  select(boecylin:zizpalus)

treatment_2018[treatment_2018 > 0] <- 1


treatment_2019 <- plant_data %>% 
  filter(treatment == "Treatment",
         sample_year == 2019) %>% 
  select(boecylin:zizpalus)

treatment_2019[treatment_2019 > 0] <- 1


treatment_2020 <- plant_data %>% 
  filter(treatment == "Treatment",
         sample_year == 2020) %>% 
  select(boecylin:zizpalus)

treatment_2020[treatment_2020 > 0] <- 1


treatment_2021 <- plant_data %>% 
  filter(treatment == "Treatment",
         sample_year == 2021) %>% 
  select(boecylin:zizpalus)

treatment_2021[treatment_2021 > 0] <- 1


# Create dataframe for BETA diversity -------------------------------------

beta_div <- data.frame(matrix(as.numeric(0), ncol=(3), nrow=(11)))
colnames(beta_div) <- c("Turnover","Nestedness","Sum")
rownames(beta_div) <- c("Control_2016", "Treatment_2016",
                        "Control_2017", "Treatment_2017",
                        "Control_2018", "Treatment_2018",
                        "Control_2019", "Treatment_2019",
                        "Control_2020", "Treatment_2020",
                        "Treatment_2021")

# Beta Diversity calculations ---------------------------------------------

# 2016 Beta Diversity 
control_2016_bd <- beta.multi(control_2016, index.family = "sorensen")
beta_div[1,] <- data.frame(matrix(unlist(control_2016_bd), nrow = length(1), byrow = T))

treatment_2016_bd <- beta.multi(treatment_2016, index.family = "sorensen")
beta_div[2,] <- data.frame(matrix(unlist(treatment_2016_bd), nrow = length(1), byrow = T))


# 2017 Beta Diversity 
control_2017_bd <- beta.multi(control_2017, index.family = "sorensen")
beta_div[3,] <- data.frame(matrix(unlist(control_2017_bd), nrow = length(1), byrow = T))

treatment_2017_bd <- beta.multi(treatment_2017, index.family = "sorensen")
beta_div[4,] <- data.frame(matrix(unlist(treatment_2017_bd), nrow = length(1), byrow = T))


# 2018 Beta Diversity 
control_2018_bd <- beta.multi(control_2018, index.family = "sorensen")
beta_div[5,] <- data.frame(matrix(unlist(control_2018_bd), nrow = length(1), byrow = T))

treatment_2018_bd <- beta.multi(treatment_2018, index.family = "sorensen")
beta_div[6,] <- data.frame(matrix(unlist(treatment_2018_bd), nrow = length(1), byrow = T))

# 2019 Beta Diversity 
control_2019_bd <- beta.multi(control_2019, index.family = "sorensen")
beta_div[7,] <- data.frame(matrix(unlist(control_2019_bd), nrow = length(1), byrow = T))

treatment_2019_bd <- beta.multi(treatment_2019, index.family = "sorensen")
beta_div[8,] <- data.frame(matrix(unlist(treatment_2019_bd), nrow = length(1), byrow = T))


# 2020 Beta Diversity 
control_2020_bd <- beta.multi(control_2020, index.family = "sorensen")
beta_div[9,] <- data.frame(matrix(unlist(control_2020_bd), nrow = length(1), byrow = T))

treatment_2020_bd <- beta.multi(treatment_2020, index.family = "sorensen")
beta_div[10,] <- data.frame(matrix(unlist(treatment_2020_bd), nrow = length(1), byrow = T))

# 2021 Beta Diversity 
treatment_2021_bd <- beta.multi(treatment_2021, index.family = "sorensen")
beta_div[11,] <- data.frame(matrix(unlist(treatment_2021_bd), nrow = length(1), byrow = T))


# Print Beta Diversity Dataframe ------------------------------------------

beta_div <- rownames_to_column(beta_div, "site_info")

beta_div <- beta_div %>% 
  separate(site_info, c("Treatment", "Year"),
           sep = "_", remove = FALSE)

write.csv(beta_div, "Data/beta_diversity_table.csv",
          row.names = FALSE)
