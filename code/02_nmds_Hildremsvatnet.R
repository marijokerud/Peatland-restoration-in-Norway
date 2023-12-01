library(vegan)

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
  mutate(AAR = gsub("2018", "0", AAR)) %>% #Gi verdi 0
  mutate(AAR = gsub("2021", "1", AAR)) %>% #Gi verdi 1
  mutate(AAR = recode_factor(AAR,
                             "0" = "0",
                             "1" = "1"))


#SPECIES SCORES
Species.scores$species <- rownames(Species.scores)  # create a column of site names, from the rownames of data.scores
species.scores <- Species.scores %>% 
  mutate(speciesNEW = species) %>% 
  mutate(art1 = str_sub(speciesNEW, start = 1, end = 4)) %>%
  mutate(art2 = speciesNEW) %>% 
  mutate(art2 = sub("^\\S+\\s+", '', speciesNEW)) %>% 
  mutate(art2 = str_sub(art2, start = 1, end = 4)) %>% 
  mutate(species = paste(art1, art2)) 



