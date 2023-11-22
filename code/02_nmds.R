library(vegan)

#Ordinasjon for hver 10 m

pinpoint_dist <-vegdist(pinpoint_mat, method = "jaccard", binary = TRUE)  #Hvorfor gjorde jeg dette?
pinpoint_dist2 <-vegdist(pinpoint_mat, method = "bray")

pin_NMDS=metaMDS(pinpoint_mat, # Our community-by-species matrix
                 k=2, trymax = 1000, distance = "jaccard") # The number of reduced dimensions, "jaccard"

stressplot(pin_NMDS)

plot(pin_NMDS)

ordiplot(pin_NMDS,type="n")
orditorp(pin_NMDS,display="species",col="red",air=0.5)
orditorp(pin_NMDS,display="sites",cex=0.8,air=0.8)


#Calculate scores 
Point.scores <- as.data.frame(scores(pin_NMDS)$sites)  #Using the scores function from vegan to extract the site scores and convert to a data.frame
Species.scores <- as.data.frame(scores(pin_NMDS)$species)


#AFTER NMDS
#AVSTAND fra grøft, Kaldvassmyra
plassering <- plassering %>% 
  select(Artslinje_id, Meter_from_ditch) %>% 
  mutate(Meter_from_ditch = gsub("160", "Referanselinje", Meter_from_ditch)) %>%  #change distance for K5 to K5
  mutate(Meter_from_ditch = gsub("170", "Referanselinje", Meter_from_ditch)) %>%  #change distance for K5 to K5
  mutate(Meter_from_ditch = gsub("180", "Referanselinje", Meter_from_ditch)) %>%  #change distance for K5 to K5
  mutate(Meter_from_ditch = gsub("190", "Referanselinje", Meter_from_ditch)) %>%  #change distance for K5 to K5
  mutate(Meter_from_ditch = gsub("200", "Referanselinje", Meter_from_ditch))   #change distance for K5 to K5

#AVSTAND fra grøft, Aurstadmåsan
plassering <- plassering %>% 
  select(Artslinje_id, Meter_from_ditch) %>% 
  mutate(Meter_from_ditch = gsub("A5", "Referanselinje", Meter_from_ditch))

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
  #mutate(Meter_from_ditch = as.numeric(Meter_from_ditch)) #USE # for regression and regression plot
  mutate(Meter_from_ditch = as.factor(Meter_from_ditch))   #USE # for NMDS plot

#Aurstadmåsan for regression
point.scores <- all.point.scores %>% 
  slice(1:61) %>% 
  select(-Meter_from_ditch) %>%   #Remove Referanselinja
  mutate(Meter_from_ditch = as.numeric(Meter_from_ditch_org))

#Kaldvassmyra for regression
point.scores <- all.point.scores %>% 
  slice(1:78)   #Remove Referanselinja


#SPECIES SCORES
Species.scores$species <- rownames(Species.scores)  # create a column of site names, from the rownames of data.scores
species.scores <- Species.scores %>% 
  mutate(speciesNEW = species) %>% 
  mutate(art1 = str_sub(speciesNEW, start = 1, end = 4)) %>%
  mutate(art2 = speciesNEW) %>% 
  mutate(art2 = sub("^\\S+\\s+", '', speciesNEW)) %>% 
  mutate(art2 = str_sub(art2, start = 1, end = 4)) %>% 
  mutate(species = paste(art1, art2)) 




####################################################
#Ordinasjon for slått sammen hele artslinjer
K_NMDS=metaMDS(com_matK, # Our community-by-species matrix
               k=2, trymax = 100) # The number of reduced dimensions
stressplot(K_NMDS)

plot(K_NMDS)

#K1<- list ("K1_2015", "K1_2018", "K1_2021")
#linjer<- as.factor(community_matrixK$community)

ordiplot(K_NMDS,type="n")
orditorp(K_NMDS,display="species",col="red",air=0.01)
orditorp(K_NMDS,display="sites",cex=1.2,air=0.01)




