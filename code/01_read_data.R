#LOAD DATA
library(readxl)
library(tidyverse)
library(labdsv)
library(xlsx)

Sys.setlocale("LC_ALL", "Norwegian") #works with æøå or use "no_NB.utf8"

artslinjer.raw <- read_excel(path = "data/Data_Kaldvassmyra.xlsx", sheet = "Data", col_names = TRUE)
artslinjer.raw <- read_excel(path = "data/Data_Hildremsvatnet.xlsx", sheet = "Data", col_names = TRUE)

artslinjer.raw <- read_excel(path = "data/Data_myr_restaurering.xlsx", sheet = "Aurstadmåsan", col_names = TRUE)
artsnavn <- read_excel(path = "data/Data_myr_restaurering.xlsx", sheet = "artsnavn1", col_names = TRUE)

tv_verdi <- read_excel(path = "data/Data_Kaldvassmyra.xlsx", sheet = "Ind.verdi_GAD_TV",range = "A1:L53" , col_names = TRUE)
plassering <- read_excel(path = "data/Data_Kaldvassmyra.xlsx", sheet = "Plassering", col_names = TRUE)

# DATA CLEANING

#Only Hildremsvatnet remove H from Artslinje_id and add 0 at the end
artslinjer2018 <- artslinjer.raw %>% 
  filter(AAR == "2018") %>% 
  rename(Artslinje_id_old = Artslinje_id) %>% 
  mutate(linje1 = str_sub(Artslinje_id_old, start = 1, end = 3)) %>%
  mutate(linje2 = str_sub(Artslinje_id_old, start = 5, end = 5)) %>%
  unite("Artslinje_id", linje1,linje2, sep = "")

artslinjer2018.1 <- artslinjer2018 %>% 
  filter(Artslinje_id == "H1_0" | Artslinje_id == "H2_0" | Artslinje_id == "H3_0" | Artslinje_id == "H4_0") 
  
artslinjer2018.2 <- artslinjer2018 %>% 
  mutate(linje0 = str_sub(Artslinje_id, start = 4, end = 4)) %>%
  filter(linje0 == "1" | linje0 == "2" | linje0 == "3" | linje0 == "4") %>% 
  mutate(Artslinje_id = paste(Artslinje_id, 0, sep = ""))

artslinjer <- artslinjer %>% 
  filter(AAR == "2021") %>%  #ADD # for analysis for the report
  bind_rows(artslinjer2018.1, artslinjer2018.2)


#KALDVASSMYRA M 2023 OG UTEN 2021
artslinjer <- artslinjer.raw %>% 
  select(AAR, OMRADE, Artslinje_id, Art, cm, AAR2, Treatment) %>% 
  filter(AAR== 2015 | AAR== 2018) %>% 
  bind_rows(kaldvassmyra2023)

#sjekk artsnavn
artsnavntest <-artslinjer %>% 
  select(Art) %>% 
  unique()


#AURSTADMÅSAN M 2023 OG UTEN 2021
artslinjer <- artslinjer.raw %>% 
  select(AAR, OMRADE, Artslinje_id, Art, cm) %>%
  rename(Artslinje_id_old = Artslinje_id) %>% 
  mutate(linje1 = str_sub(Artslinje_id_old, start = 1, end = 3)) %>%
  mutate(linje2 = str_sub(Artslinje_id_old, start = 5, end = 6)) %>%
  unite("Artslinje_id", linje1,linje2, sep = "") %>% 
  filter(AAR== 2015 | AAR== 2018) %>% 
  right_join(artsnavn, by ="Art") %>% 
  select(AAR, OMRADE, Artslinje_id, Art2, cm) %>%
  rename(Art = Art2) %>% 
  bind_rows(aurstadmosan2023) 

#sjekk artsnavn
artsnavntest <-artslinjer %>% 
  select(Art2) %>% 
  unique()



#### COMMUNITY MATRIX every 10 m
pinpoint_matrix<- artslinjer %>% 
  unite("community", Artslinje_id, AAR) %>% 
  select(community, Art) %>% 
  distinct() %>% 
  mutate(Abundance = 1) %>%
  as.data.frame

#Hildremsvatnet
pinpoint_mat<- matrify(pinpoint_matrix)
pinpoint_mat <- pinpoint_mat %>% 
  select(-`Bare peat`, -Litter, -Water, -Wood)
pinpoint_mat<- pinpoint_mat[-30,] #Remove H3_40_2021 because of zeros, only water and bare peat

#Kaldvassmyra
pinpoint_mat<- matrify(pinpoint_matrix)
pinpoint_mat <- pinpoint_mat %>% 
  #select(-litter, -water) %>% #-Litter, -dead_sph,-dead_wood,-peat, -water #  #-Litter, -dead_sph,-dead_wood,-peat, -water
  select(-`dead wood`, -litter, -`open water`, -Strø, -Torv, -water) %>%
  filter(!row_number() %in% c(54)) %>%  # #REMOVE column with only 0s: K3_40_2018 (66)  %in% c(10, 66))
  slice(1:78)  #Remove K5

pinpoint_matKALD <- pinpoint_mat %>%
  slice(1:68) #Remove K5

#Aurstadmåsan
pinpoint_mat<- matrify(pinpoint_matrix)
pinpoint_mat <- pinpoint_mat %>% 
  #select(-litter, -water) %>% #-Litter, -dead_sph,-dead_wood,-peat, -water #  #-Litter, -dead_sph,-dead_wood,-peat, -water
  select(-dead_sph, -peat, -Strø, -Torv) %>%
  filter(!row_number() %in% c(9,12,27,55))  # #REMOVE column with only 0s: A1_20_2023 (9), A1_30_2023 (12), A2_40_2023 (27), A4_30_2023 (55)

#DO NMDS


#AFTER NMDS
#AVSTAND fra grøft, gjelder bare Kaldvassmyra
plassering_short <- plassering %>% 
  select(Artslinje_id, Meter_from_ditch) %>% 
  mutate(Meter_from_ditch = gsub("160", "K5", Meter_from_ditch)) %>%  #change distance for K5 to K5
  mutate(Meter_from_ditch = gsub("170", "K5", Meter_from_ditch)) %>%  #change distance for K5 to K5
  mutate(Meter_from_ditch = gsub("180", "K5", Meter_from_ditch)) %>%  #change distance for K5 to K5
  mutate(Meter_from_ditch = gsub("190", "K5", Meter_from_ditch)) %>%  #change distance for K5 to K5
  mutate(Meter_from_ditch = gsub("200", "K5", Meter_from_ditch))   #change distance for K5 to K5
#ÅR siden tiltak

#SITE SCORES etter NMDS
Point.scores$point <- rownames(Point.scores)  # create a column of site names, from the rownames of data.scores
all.point.scores <- Point.scores %>% 
  mutate(Artslinje_id = point) %>% 
  mutate(Artslinje_id = str_sub(Artslinje_id, end = -6)) %>% 
  #mutate(Artslinje_id = gsub("_2015", "", Artslinje_id)) %>% #remove _2015
  #mutate(Artslinje_id = gsub("_2018", "", Artslinje_id)) %>% #remove _2018
  #mutate(Artslinje_id = gsub("_2023", "", Artslinje_id)) %>%  #remove _2021
  mutate(linje = point) %>% 
  mutate(linje = str_sub(linje, start = 1, end = 2)) %>% 
  mutate(AAR2 = point) %>% 
  mutate(AAR2 = str_sub(AAR2, -4)) %>% 
  mutate(AAR = AAR2) %>% 
  mutate(AAR = gsub("2015", "0", AAR)) %>% #Gi verdi 0
  mutate(AAR = gsub("2018", "1", AAR)) %>% #Gi verdi 1
  mutate(AAR = gsub("2023", "2", AAR)) %>% #Gi verdi 2
  mutate(AAR = recode_factor(AAR,
                             "0" = "0",
                             "1" = "1",
                             "2" = "2")) %>% 
  left_join(plassering_short) %>% 
  mutate(Meter_from_ditch = as.numeric(Meter_from_ditch)) #ADD # when creating the figure

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

#AURSTADMÅSAN
A5 <- all.point.scores %>% 
  slice(62:76)

mean5 <- all.point.scores %>% 
  slice(62:76) %>% 
  summarise(avg = mean(NMDS1))

sd5 <- all.point.scores %>% 
  slice(62:76) %>% 
  summarise(SD= sd(NMDS1))

K5 <- all.point.scores %>% 
  slice(79:83) 

mean5 <- all.point.scores %>% 
  slice(79:83) %>% 
  summarise(avg = mean(NMDS1))

K5mean <- c(0.1852915+ -0.3034232+ -0.7542850+ -0.2466182+ -0.2962386)/5
K5mean<- c(0.1852915, -0.3034232, -0.7542850, -0.2466182, -0.2962386)
mean(K5mean)
sd(K5mean)

model5<- lm(NMDS1~1, K5)

confint(model5, level = 0.95)



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
  
  
  