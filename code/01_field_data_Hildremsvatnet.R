#LOAD DATA
library(readxl)
library(tidyverse)
library(labdsv)
library(xlsx)

Sys.setlocale("LC_ALL", "Norwegian") #works with æøå or use "no_NB.utf8"

artslinjer.raw <- read_excel(path = "data/Data_Hildremsvatnet.xlsx", sheet = "Data", col_names = TRUE)

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

artslinjer <- artslinjer.raw %>% 
  filter(AAR == "2021") %>%  #ADD # for analysis for the report
  bind_rows(artslinjer2018.1, artslinjer2018.2) %>% 
  select(AAR, OMRADE, Artslinje_id, Art, cm, AAR2)


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
  select(-'Bare peat', -Litter, -Water, -Wood) %>% 
  filter(!row_number() %in% c(30)) #REMOVE column with only 0s: H3_40_2021 consists only water and bare peat

#DO NMDS


write_csv2(artslinjer, file = "Hildremsvatnet.csv", na = "NA")
