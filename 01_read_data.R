library(readxl)


artslinjer <- read_excel(path = "Data Kaldvassmyra.xlsx", sheet = "Data", col_names = TRUE)
tv_verdi <- read_excel(path = "Data Kaldvassmyra.xlsx", sheet = "Ind.verdi_GAD_TV",range = "A1:L53" , col_names = TRUE)
plassering <- read_excel(path = "Data Kaldvassmyra.xlsx", sheet = "Plassering", col_names = TRUE)

# DATA CLEANING
library(tidyverse)
library(labdsv)

# COMMUNITY MATRIX every 10 m
pinpoint_matrix<- artslinjer %>% 
  unite("community", Artslinje_id, AAR) %>% 
  select(community, Art) %>% 
  distinct() %>% 
  mutate(Abundance = 1) %>%
  as.data.frame

pinpoint_mat<- matrify(pinpoint_matrix)
pinpoint_mat <- pinpoint_mat %>% 
  select(-Litter,-litter,-dead_sph,-dead_wood,-peat, -water) %>% 
  filter(!row_number() %in% c(10, 46)) #REMOVE column with only 0, 46 and column 10 with Hylocomium.

pinpoint_matRED <- pinpoint_mat %>%
  slice(1:69)


# COMMUNITY MATRIX per line
community_matrixK<- artslinjer %>% 
  unite("community", LINJE, AAR) %>% 
  select(community, Art) %>% 
  distinct() %>% 
  mutate(Abundance = 1) %>% 
  as.data.frame

com_matK<- matrify(community_matrixK) 

#AVSTAND fra grøft
plassering_short <- plassering %>% 
  select(Artslinje_id, Meter_from_ditch)
#ÅR siden tiltak

#SITE SCORES
Point.scores$point <- rownames(Point.scores)  # create a column of site names, from the rownames of data.scores
point.scores <- Point.scores %>% 
  mutate(Artslinje_id = point) %>% 
  mutate(Artslinje_id = gsub("_2015", "", Artslinje_id)) %>% #remove _2015
  mutate(Artslinje_id = gsub("_2018", "", Artslinje_id)) %>% #remove _2018
  mutate(Artslinje_id = gsub("_2021", "", Artslinje_id)) %>%  #remove _2021
  mutate(AAR2 = point) %>% 
  mutate(AAR2 = str_sub(AAR2, -4)) %>% 
  mutate(AAR = AAR2) %>% 
  mutate(AAR = gsub("2015", "0", AAR)) %>% #Gi verdi 0
  mutate(AAR = gsub("2018", "1", AAR)) %>% #Gi verdi 1
  mutate(AAR = gsub("2021", "2", AAR)) %>% #Gi verdi 2
  mutate(AAR = recode_factor(AAR,
                              "0" = "0",
                              "1" = "1",
                              "2" = "2")) %>% 
  left_join(plassering_short) 
  #slice(1:68) #Remove K5
