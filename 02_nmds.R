library(vegan)

#Ordinasjon for hver 10 m

pin_NMDS=metaMDS(pinpoint_mat, # Our community-by-species matrix
               k=2, trymax = 100, distance = "bray") # The number of reduced dimensions
stressplot(pin_NMDS)

plot(pin_NMDS)

ordiplot(pin_NMDS,type="n")
orditorp(pin_NMDS,display="species",col="red",air=0.5)
orditorp(pin_NMDS,display="sites",cex=1.2,air=0.8)

#Calculate scores 
Site.scores <- as.data.frame(scores(pin_NMDS)$sites)  #Using the scores function from vegan to extract the site scores and convert to a data.frame


#Plott=c(rep("Kystlynghei_G(F)",5), rep("Fastmatte_G",10), rep("Tue_G",10), rep("Kystlynghei_G",10), rep("Tue_L",10), rep("Fastmatte_L",10), rep("Kystlynghei_L",10), rep("Kystlynghei_G(F)",5) )
#data.scores$Plott <- Plott  #  add the plott variable created earlier
#head(data.scores)


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




