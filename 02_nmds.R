library(vegan)
K_NMDS=metaMDS(com_matK, # Our community-by-species matrix
               k=2, trymax = 100) # The number of reduced dimensions
stressplot(K_NMDS)


plot(K_NMDS)

#K1<- list ("K1_2015", "K1_2018", "K1_2021")
#linjer<- as.factor(community_matrixK$community)

ordiplot(K_NMDS,type="n")
orditorp(K_NMDS,display="species",col="red",air=0.01)
orditorp(K_NMDS,display="sites",cex=1.2,air=0.01)


