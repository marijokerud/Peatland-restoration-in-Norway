#LOAD DATA
library(readxl)
library(tidyverse)
library(labdsv)
library(xlsx)

artslinjer.raw <- read.csv(file = "data/Aurstadmåsan2015_2018.csv", sep = ";")
feltskjema_aurstadmosan <- read.csv(file = "data/Aurstadmåsan2023.csv", sep = ";")
artsnavn <- read.csv(file = "data/Artsnavn.csv", sep = ";")
plassering <- read_excel(path = "data/Data_myr_restaurering.xlsx", sheet = "PlasseringA", col_names = TRUE)


aurstadmosan2023 <- feltskjema_aurstadmosan %>%
  gather("cm", "presence", -year, -site, -field_analyst, -transect_id, -species) %>% 
  mutate(site = gsub("Aurstadmasan", "Aurstadmosen", site)) %>%
  mutate(cm = str_sub(cm, start = 2, end = 4)) %>%
  mutate(cm = as.numeric(cm)) %>% 
  drop_na() %>% 
  select(-presence, -field_analyst) %>% 
  rename(AAR = year, OMRADE = site, Artslinje_id = transect_id, Art = species)

#AURSTADMÅSAN M 2023 OG UTEN 2021
artslinjer <- artslinjer.raw %>% 
  select(AAR, OMRADE, Artslinje_id, Art, cm) %>%
  rename(Artslinje_id_old = Artslinje_id) %>% 
  mutate(linje1 = str_sub(Artslinje_id_old, start = 1, end = 3)) %>%
  mutate(linje2 = str_sub(Artslinje_id_old, start = 5, end = 6)) %>%
  unite("Artslinje_id", linje1,linje2, sep = "") %>% 
  right_join(artsnavn, by ="Art") %>% 
  select(AAR, OMRADE, Artslinje_id, Art2, cm) %>%
  rename(Art = Art2) %>% 
  bind_rows(aurstadmosan2023) %>% 
  mutate(Art2 = gsub("\\s+", "_", perl=TRUE, Art)) %>% 
  mutate(Art = gsub("_", " ", fixed=TRUE, Art2)) %>% 
  select(-Art2)

#sjekk artsnavn
artsnavntest <-artslinjer %>% 
  select(Art) %>% 
  unique()

plass <- artslinjer %>% 
  select(Artslinje_id) %>% 
  unique()


#### COMMUNITY MATRIX every 10 m
pinpoint_matrix<- artslinjer %>% 
  unite("community", Artslinje_id, AAR) %>% 
  select(community, Art) %>% 
  distinct() %>% 
  mutate(Abundance = 1) %>%
  as.data.frame


#Aurstadmåsan
pinpoint_mat<- matrify(pinpoint_matrix)
pinpoint_mat <- pinpoint_mat %>% 
  #select(-litter, -water) %>% #-Litter, -dead_sph,-dead_wood,-peat, -water #  #-Litter, -dead_sph,-dead_wood,-peat, -water
  select(-`Dead sph`, -Strø, -Torv)

#DO NMDS


#AFTER NMDS

#SITE SCORES etter NMDS
Point.scores$point <- rownames(Point.scores)  # create a column of site names, from the rownames of data.scores
all.point.scores <- Point.scores %>% 
  mutate(Artslinje_id = point) %>% 
  mutate(Artslinje_id = str_sub(Artslinje_id, end = -6)) %>% 
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
  left_join(plassering) %>% 
  mutate(Meter_from_ditch2 = as.factor(Meter_from_ditch)) %>%  #USE this when creating the figure
  mutate(Meter_from_ditch = as.numeric(Meter_from_ditch))  #ADD # when creating the figure


point.scores <- all.point.scores %>% 
  slice(1:61)  #Remove A5


#SPECIES SCORES
Species.scores$species <- rownames(Species.scores)  # create a column of site names, from the rownames of data.scores
species.scores <- Species.scores %>% 
  mutate(speciesNEW = species) %>% 
  mutate(art1 = str_sub(speciesNEW, start = 1, end = 4)) %>%
  mutate(art2 = speciesNEW) %>% 
  mutate(art2 = sub("^\\S+\\s+", '', speciesNEW)) %>% 
  mutate(art2 = str_sub(art2, start = 1, end = 4)) %>% 
  mutate(species = paste(art1, art2)) 
