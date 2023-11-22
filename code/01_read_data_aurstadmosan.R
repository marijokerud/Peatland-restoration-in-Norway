#LOAD DATA
library(readxl)
library(tidyverse)
library(labdsv)
library(xlsx)

artslinjer.raw <- read.csv(file = "data/Aurstadmåsan2015_2018.csv", sep = ";")
feltskjema_aurstadmosan <- read.csv(file = "data/Aurstadmåsan2023.csv", sep = ";")
artsnavn <- read.csv(file = "data/Artsnavn.csv", sep = ";")


artslinjer.raw <- read_excel(path = "data/Data_myr_restaurering.xlsx", sheet = "Aurstadmåsan", col_names = TRUE)
artsnavn <- read_excel(path = "data/Data_myr_restaurering.xlsx", sheet = "artsnavnA", col_names = TRUE)
feltskjema_aurstadmosan <- read_excel(path = "data/Data_myr_restaurering.xlsx", sheet = "Aurstadmåsan2023", col_names = TRUE)
plassering <- read_excel(path = "data/Data_myr_restaurering.xlsx", sheet = "PlasseringA", col_names = TRUE)


aurstadmosan2023 <- feltskjema_aurstadmosan %>%
  gather("cm", "presence", -year, -site, -field_analyst, -transect_id, -species) %>% 
  mutate(site = gsub("Aurstadmasan", "Aurstadmosen", site)) %>%
  mutate(cm = str_sub(cm, start = 2, end = 4)) %>%
  mutate(cm = as.numeric(cm)) %>% 
  drop_na() %>% 
  select(-presence, -field_analyst) %>% 
  rename(AAR = year, OMRADE = site, Artslinje_id = transect_id, Art = species)
  #mutate(Art2 = gsub("\\s+", "_", perl=TRUE, Art))

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
  bind_rows(aurstadmosan2023) 



#sjekk artsnavn
artsnavntest <-artslinjer %>% 
  select(Art) %>% 
  unique()

plass <- artslinjer %>% 
  select(Artslinje_id) %>% 
  unique()


#### COMMUNITY MATRIX every 10 m
pinpoint_matrix<- artslinjer  %>% #OR artslinjerFULL for NMDS plot OR artslinjer for regression analysis and plot
  unite("community", Artslinje_id, AAR) %>% 
  select(community, Art) %>% 
  distinct() %>% 
  mutate(Abundance = 1) %>%
  as.data.frame


#Aurstadmåsan
pinpoint_mat<- matrify(pinpoint_matrix)
pinpoint_mat <- pinpoint_mat %>% 
  select(-Dead_sph, -Strø, -Torv)

#DO NMDS




######################
mutate(Art2 = gsub("\\s+", "_", perl=TRUE, Art)) %>% 
  mutate(Art = gsub("_", " ", fixed=TRUE, Art2)) %>% 
  select(-Art2)


#UTEN H5 for regresjonsplott
artslinjer <- artslinjerFULL %>% 
  mutate(linje = Artslinje_id) %>% 
  mutate(linje = str_sub(linje, start = 1, end = 2)) %>% 
  filter(linje== "A1" | linje== "A2" | linje== "A3" | linje== "A4")