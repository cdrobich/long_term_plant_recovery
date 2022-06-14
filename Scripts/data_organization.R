
# Libraries ---------------------------------------------------------------

library(tidyverse)

# Data --------------------------------------------------------------------

efficacy_data <- read.csv("Raw_data/all_efficacy.csv")

# clean up the column names

efficacy_data <- janitor::clean_names(efficacy_data)
colnames(efficacy_data)


# remove summary information (S and CC)

efficacy_data <- efficacy_data %>% 
  select(site_code:unknown_seedling)

## select only the vegetation data
plant_community <- efficacy_data %>% 
  select(water:unknown_seedling)

# check for empty columns ( no species)

empty_columns <- efficacy_data %>% 
  select_if(is.numeric) %>% 
  select_if(colSums(.) == 0)

# empty columns that were removed, create a vector of the names to remove
target <- c(colnames(empty_columns))
# [1] "bare_ground"   "bryophyte_sp"  "calystegia_sp" "calsepiu"      "carbuxba"      "carcrawe"     
# [7] "carcrypt"      "carlangu"      "carsartw"      "clamaris"      "cornus_sp"     "eleocharis_sp"
# [13] "epuatorium_sp" "fern"          "fravirgi"      "hypkalmi"      "iriversa"      "junbalti"     
# [19] "junbrevi"      "lycameri"      "lytsalic"      "rosa_sp"       "vicia_sp" 


#  remove the empty columns (specified in "target")
plant_data <- efficacy_data %>% 
  select(-c(target))

write.csv(plant_data, "Data/vegetation_data.csv", row.names = FALSE)



# Sparsity ----------------------------------------------------------------


# species that occur <= 2 times
sparse_species <- plant_data %>% 
  select(achmille:unknown_seedling) %>% 
  select_if(colSums(.) <= 2) %>% 
  colnames()

# [1] "achmille"      "camapari"      "carlacus"      "gallabra"      "juncus_sp"     "leeoryzo"     
# [7] "lysthrys"      "lythrum_sp"    "potgrami"      "schpunge"      "solonaceae_sp" "utrmino"   

# remove species that have 2 or fewer occurrences 
plant_sparsity <- plant_data %>%
  select(-c(sparse_species))

write.csv(plant_sparsity, "Data/vegetation_data_sparsity.csv", row.names = FALSE)


