#LOAD DATA
library(readxl)
library(tidyverse)
library(labdsv)
library(xlsx)

Sys.setlocale("LC_ALL", "Norwegian") #works with æøå or use "no_NB.utf8"

artslinjer.raw <- read_excel(path = "data/Data_Kaldvassmyra.xlsx", sheet = "Data", col_names = TRUE)

feltskjema_kaldvassmyra <- read_excel(path = "data/Data_myr_restaurering.xlsx", sheet = "Kaldvassmyra2023", col_names = TRUE)
tv_verdi <- read_excel(path = "data/Data_Kaldvassmyra.xlsx", sheet = "Ind.verdi_GAD_TV",range = "A1:L53" , col_names = TRUE)
plassering <- read_excel(path = "data/Data_Kaldvassmyra.xlsx", sheet = "Plassering", col_names = TRUE)

# DATA CLEANING
kaldvassmyra2023 <- feltskjema_kaldvassmyra %>%
  gather("CM", "presence", -year, -site, -field_analyst, -transect_id, -species) %>% 
  mutate("cm"= paste(CM, 0, sep = "")) %>% 
  mutate(cm = as.numeric(cm)) %>% 
  mutate(species = gsub("Betula nana B.", "Betula nana", species)) %>% 
  mutate(species = gsub("Betula nana C.", "Betula nana", species)) %>%
  mutate(species = gsub("Betula nana F.", "Betula nana", species)) %>%
  mutate(species = gsub("Picea abies B.", "Picea abies", species)) %>%
  mutate(species = gsub("Pinus sylvestris B.", "Pinus sylvestris", species)) %>%
  mutate(species = gsub("Pinus sylvestris C.", "Pinus sylvestris", species)) %>%
  mutate(AAR2 = 6) %>% 
  mutate(Treatment = "after3") %>% 
  drop_na() %>% 
  select(-CM, -presence, -field_analyst) %>% 
  rename(AAR = year, OMRADE = site, Artslinje_id = transect_id, Art = species)


#KALDVASSMYRA M 2023 OG UTEN 2021
artslinjer <- artslinjer.raw %>% 
  select(AAR, OMRADE, Artslinje_id, Art, cm, AAR2, Treatment) %>% 
  filter(AAR== 2015 | AAR== 2018) %>% 
  bind_rows(kaldvassmyra2023)


#sjekk artsnavn
artsnavntest <-artslinjer %>% 
  select(Art) %>% 
  unique()


#### COMMUNITY MATRIX every 10 m
pinpoint_matrix<- artslinjer  %>% #OR artslinjerFULL for NMDS plot OR artslinjer for regression analysis and plot
  unite("community", Artslinje_id, AAR) %>% 
  select(community, Art) %>% 
  distinct() %>% 
  mutate(Abundance = 1) %>%
  as.data.frame


pinpoint_mat<- matrify(pinpoint_matrix)
pinpoint_mat <- pinpoint_mat %>% 
  select(-`dead wood`, -litter, -`open water`, -Strø, -Torv, -water) %>%
  filter(!row_number() %in% c(54))  # #REMOVE column with only 0s: K3_40_2018 



#DO NMDS






#####################################################################
## Functional group
fungroupK <- artslinjer %>% 
  select(Art, Funksjonell_gruppe) %>% 
  distinct()





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
  
  
  #UTEN K5 for regresjonsplott
  artslinjer <- artslinjerFULL %>% 
    mutate(linje = Artslinje_id) %>% 
    mutate(linje = str_sub(linje, start = 1, end = 2)) %>% 
    filter(linje== "K1" | linje== "K2" | linje== "K3" | linje== "K4")
  
  
  