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

cm<- c(10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,210,220,230,240,250)
cm2<- c(120,120,120,120,120,120,120,120,120,120,120,120,
        250,250,250,250,250,250,250,250,250,250,250,250,250)
breakdata<- data.frame(cm, cm2)

# COMMUNITY MATRIX every 10 m, 0-120 cm og 130-250 cm
pinpoint_matrix2<- artslinjer %>% 
  unite("community", Artslinje_id, AAR) %>% 
  select(community, Art, cm) %>% 
  left_join(breakdata) %>% 
  select(-cm) %>% 
  distinct() %>% 
  mutate(Abundance = 1) %>%
  unite(comunityNEW, community, cm2) %>% 
  as.data.frame

pinpoint_mat<- matrify(pinpoint_matrix)
pinpoint_mat <- pinpoint_mat %>% 
  select(-Litter,-litter,-dead_sph,-dead_wood,-peat, -water) %>% 
  filter(!row_number() %in% c(10, 46)) #REMOVE column with only 0, 46 and column 10 with Hylocomium.

pinpoint_mat2<- matrify(pinpoint_matrix2)
pinpoint_mat2 <- pinpoint_mat2 %>% 
  select(-Litter,-litter,-dead_sph,-dead_wood,-peat, -water) %>% 
  filter(!row_number() %in% c(19,21,55,57,91,92,93)) #remove columns with only 0

pinpoint_matRED2 <- pinpoint_mat2 %>%
  slice(1:133)


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
#Point.scores2$point <- rownames(Point.scores2)
point.scores <- Point.scores %>% 
  mutate(Artslinje_id = point) %>% 
  mutate(Artslinje_id = str_sub(Artslinje_id, end = -4))
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

#SITE SCORES 2
Point.scores2$point <- rownames(Point.scores2)  # create a column of site names, from the rownames of data.scores
point.scores <- Point.scores2 %>% 
  mutate(Artslinje_id = point) %>% 
  mutate(Artslinje_id = str_sub(Artslinje_id, end = -10))
  mutate(Artslinje_id = gsub("_2015", "", Artslinje_id)) %>% #remove _2015
  mutate(Artslinje_id = gsub("_2018", "", Artslinje_id)) %>% #remove _2018
  mutate(Artslinje_id = gsub("_2021", "", Artslinje_id)) %>%  #remove _2021
  mutate(CM2 = point) %>% 
  mutate(CM2 = str_sub(CM2, -3)) %>% 
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
  left_join(plassering_short) 
