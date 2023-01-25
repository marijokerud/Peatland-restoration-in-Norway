library(tidyverse)
library(readxl)


artslinjer <- read_excel(path = "data/Data_Kaldvassmyra.xlsx", sheet = "Data", col_names = TRUE)
tv_verdi <- read_excel(path = "data/Data_Kaldvassmyra.xlsx", sheet = "Ind.verdi_GAD_TV",range = "A1:L53" , col_names = TRUE)
plassering <- read_excel(path = "data/Data_Kaldvassmyra.xlsx", sheet = "Plassering", col_names = TRUE)

# DATA CLEANING
library(tidyverse)
library(labdsv)

## Functional group
fungroupK <- artslinjer %>% 
  select(Art, Funksjonell_gruppe) %>% 
  distinct()

#### COMMUNITY MATRIX every 10 m
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
  slice(1:68) #Remove K5

#AVSTAND fra grøft
plassering_short <- plassering %>% 
  select(Artslinje_id, Meter_from_ditch)
#ÅR siden tiltak

#SITE SCORES
Point.scores$point <- rownames(Point.scores)  # create a column of site names, from the rownames of data.scores
all.point.scores <- Point.scores %>% 
  mutate(Artslinje_id = point) %>% 
  mutate(Artslinje_id = str_sub(Artslinje_id, end = -6)) %>% 
  mutate(Artslinje_id = gsub("_2015", "", Artslinje_id)) %>% #remove _2015
  mutate(Artslinje_id = gsub("_2018", "", Artslinje_id)) %>% #remove _2018
  mutate(Artslinje_id = gsub("_2021", "", Artslinje_id)) %>%  #remove _2021
  mutate(linje = point) %>% 
  mutate(linje = str_sub(linje, start = 1, end = 2)) %>% 
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

point.scores <- all.point.scores %>% 
  slice(1:68)  #Remove K5

K5 <- point.scores %>% 
  slice(69:73) %

K5mean <- (0.1852915+ -0.3034232+ -0.7542850+ -0.2466182+ -0.2962386)/5

modelK5<- lm(NMDS1~1, K5)

confint(modelK5, level = 0.95)

#SPECIES SCORES
Species.scores$species <- rownames(Species.scores)  # create a column of site names, from the rownames of data.scores
species.scores <- Species.scores %>% 
  mutate(speciesNEW = species) %>% 
  mutate(art1 = str_sub(speciesNEW, start = 1, end = 4)) %>%
  mutate(art2 = speciesNEW) %>% 
  mutate(art2 = sub("^\\S+\\s+", '', speciesNEW)) %>% 
  mutate(art2 = str_sub(art2, start = 1, end = 4)) %>% 
  mutate(species = paste(art1, art2))



#### COMMUNITY MATRIX every 10 m, 0-120 cm og 130-250 cm
breakdata <- tibble(
  cm = seq(10,250, 10),
  cm2 = c(rep(120, 12), rep(250,13)))


pinpoint_matrix2<- artslinjer %>% 
  unite("community", Artslinje_id, AAR) %>% 
  select(community, Art, cm) %>% 
  left_join(breakdata) %>% 
  select(-cm) %>% 
  distinct() %>% 
  mutate(Abundance = 1) %>%
  unite(comunityNEW, community, cm2) %>% 
  as.data.frame

pinpoint_mat2<- matrify(pinpoint_matrix2)
pinpoint_mat2 <- pinpoint_mat2 %>% 
  select(-Litter,-litter,-dead_sph,-dead_wood,-peat, -water) %>% 
  filter(!row_number() %in% c(19,21,55,57,91,92,93)) #remove columns with only 0

pinpoint_matRED2 <- pinpoint_mat2 %>%
  slice(1:133)


#SITE SCORES 2
Point.scores2$point <- rownames(Point.scores2)  # create a column of site names, from the rownames of data.scores
point.scores <- Point.scores2 %>% 
  mutate(Artslinje_id = point) %>% 
  mutate(Artslinje_id = str_sub(Artslinje_id, end = -10)) %>% #Remove 2015,2018,2021
  mutate(CM2 = point) %>% 
  mutate(CM2 = str_sub(CM2, -3)) %>% 
  mutate(CM2 = gsub("120", "0.0", CM2)) %>% #Gi verdi 1 eller bruk mutate(CM2= CM2/1000) %>%
  mutate(CM2 = gsub("250", "0.25", CM2)) %>% 
  mutate(CM2= as.numeric(CM2)) %>% 
  mutate(AAR2 = point) %>% 
  mutate(AAR2 = str_sub(AAR2, end = -5)) %>% 
  mutate(AAR2 = str_sub(AAR2, -4)) %>% 
  mutate(AAR = AAR2) %>% 
  mutate(AAR = gsub("2015", "0", AAR)) %>% #Gi verdi 0
  mutate(AAR = gsub("2018", "1", AAR)) %>% #Gi verdi 1
  mutate(AAR = gsub("2021", "2", AAR)) %>% #Gi verdi 2
  mutate(AAR = recode_factor(AAR,
                             "0" = "0",
                             "1" = "1",
                             "2" = "2")) %>% 
  left_join(plassering_short) %>% 
  mutate(Meter_from_ditch2= Meter_from_ditch +CM2)



  
### COMMUNITY MATRIX per line
  community_matrixK<- artslinjer %>% 
    unite("community", LINJE, AAR) %>% 
    select(community, Art) %>% 
    distinct() %>% 
    mutate(Abundance = 1) %>% 
    as.data.frame
  
  com_matK<- matrify(community_matrixK) 