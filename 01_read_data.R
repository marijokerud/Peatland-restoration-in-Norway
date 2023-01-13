library(readxl)


artslinjer <- read_excel(path = "Data Kaldvassmyra.xlsx", sheet = "Data", col_names = TRUE)
tv_verdi <- read_excel(path = "Data Kaldvassmyra.xlsx", sheet = "Ind.verdi_GAD_TV",range = "A1:L53" , col_names = TRUE)
plassering <- read_excel(path = "Data Kaldvassmyra.xlsx", sheet = "Plassering", col_names = TRUE)

# Data cleaning
library(tidyverse)

# Create community matrix every 10 m
pinpoint_matrix<- artslinjer %>% 
  unite("community", Artslinje_id, AAR) %>% 
  select(community, Art) %>% 
  mutate(Abundance = 1) %>% 
  as.data.frame



# Create community matrix per line
community_matrixK<- artslinjer %>% 
  unite("community", LINJE, AAR) %>% 
  select(community, Art) %>% 
  distinct() %>% 
  mutate(Abundance = 1) %>% 
  as.data.frame

library(labdsv)
pinpoint_mat<- matrify(pinpoint_matrix)
com_matK<- matrify(community_matrixK) 



