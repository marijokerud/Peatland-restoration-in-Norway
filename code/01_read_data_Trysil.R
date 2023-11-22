#LOAD DATA
library(readxl)
library(tidyverse)
library(labdsv)
library(xlsx)

Sys.setlocale("LC_ALL", "Norwegian") #works with æøå or use "no_NB.utf8"

artslinjer.raw <- read_excel(path = "data/Data_myr_restaurering.xlsx", sheet = "Trysil2021", col_names = TRUE)
artsnavn <- read_excel(path = "data/Data_myr_restaurering.xlsx", sheet = "artsnavnT", col_names = TRUE)
feltskjema_trysil <- read_excel(path = "data/Data_myr_restaurering.xlsx", sheet = "Trysil2023", col_names = TRUE)

#TRYSIL
trysil2023 <- feltskjema_trysil %>%
  gather("cm", "presence", -transect_id, -species) %>% 
  mutate(presence = as.numeric(gsub("x", 1, presence))) %>% 
  drop_na() %>% 
  select(-presence) %>% 
  mutate(AAR = as.numeric("2023")) %>% 
  mutate(OMRADE = "Hisåsen Regnåsen") %>%
  mutate(cm = as.numeric(cm)) %>%
  rename(Artslinje_id = transect_id, Art = species) %>% 
  select(AAR, OMRADE, Artslinje_id, Art, cm)

# DATA CLEANING
artslinjer <- artslinjer.raw %>% 
  select(AAR, OMRADE, Artslinje_id, Art, cm)  %>% 
  right_join(artsnavn, by ="Art") %>% 
  select(AAR, OMRADE, Artslinje_id, Art2, cm) %>%
  rename(Art = Art2) %>% 
  bind_rows(trysil2023)

#sjekk artsnavn
artsnavntest <-artslinjer %>%
  select(Art) %>% 
  unique()


#### COMMUNITY MATRIX every 10 m
pinpoint_matrix<- artslinjer %>% 
  unite("community", Artslinje_id, AAR) %>% 
  select(community, Art) %>% 
  distinct() %>% 
  mutate(Abundance = 1) %>%
  as.data.frame

pinpoint_mat<- matrify(pinpoint_matrix)
pinpoint_mat <- pinpoint_mat %>% 
  select(-Dead_sph, -Strø, -Torv)
  

pinpoint_matKALD <- pinpoint_mat %>%
  slice(1:68) #Remove K5


#DO NMDS


#AFTER NMDS

#SITE SCORES 
Point.scores$point <- rownames(Point.scores)  # create a column of site names, from the rownames of data.scores
all.point.scores <- Point.scores %>% 
  mutate(Artslinje_id = point) %>% 
  mutate(Artslinje_id = str_sub(Artslinje_id, end = -6)) %>% 
  mutate(linje = point) %>% 
  mutate(linje = str_sub(linje, start = 1, end = 3)) %>% 
  mutate(AAR2 = point) %>% 
  mutate(AAR2 = str_sub(AAR2, -4)) %>% 
  mutate(AAR = AAR2) %>% 
  mutate(AAR = gsub("2021", "0", AAR)) %>% #Gi verdi 0
  mutate(AAR = gsub("2023", "1", AAR)) %>% #Gi verdi 1
  mutate(AAR = recode_factor(AAR,
                             "0" = "0",
                             "1" = "1"))

#point.scores <- all.point.scores %>% 
  #slice(1:68)  #Remove K5


#SPECIES SCORES
Species.scores$species <- rownames(Species.scores)  # create a column of site names, from the rownames of data.scores
species.scores <- Species.scores %>% 
  mutate(speciesNEW = species) %>% 
  mutate(art1 = str_sub(speciesNEW, start = 1, end = 4)) %>%
  mutate(art2 = speciesNEW) %>% 
  mutate(art2 = sub("^\\S+\\s+", '', speciesNEW)) %>% 
  mutate(art2 = str_sub(art2, start = 1, end = 4)) %>% 
  mutate(species = paste(art1, art2)) 


## Functional group
fungroupK <- artslinjer %>% 
  select(Art, Funksjonell_gruppe) %>% 
  distinct()



K5 <- all.point.scores %>% 
  slice(69:73) 

K5mean <- c(0.1852915+ -0.3034232+ -0.7542850+ -0.2466182+ -0.2962386)/5
K5mean<- c(0.1852915, -0.3034232, -0.7542850, -0.2466182, -0.2962386)
mean(K5mean)
sd(K5mean)

modelK5<- lm(NMDS1~1, K5)

confint(modelK5, level = 0.95)



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
  
  
############################## LEFT OVER CODE ##############################

  test<- artslinjer %>% 
    group_by(Art) %>% 
    summarise_all(funs(trimws(paste(., collapse = ''))))
  
  write.table(artslinjer, file= "data/Data_Aurstadmosan.txt", sep = ";", dec = ".")
  
  write.xlsx(artslinjer, file= "data/Data_Aurstadmosan.xlsx", sheetName = "Sheet1", 
             col.names = TRUE, row.names = TRUE, append = FALSE)
  artslinjer <- read_excel(path = "data/Data_Aurstadmosan.xlsx", sheet = "Sheet1", col_names = TRUE)
  
  
  