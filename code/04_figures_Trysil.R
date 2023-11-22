library(ggplot2)
library(ggrepel)
library(ggpattern)
#library(wesanderson)


myfill <- c("#7b3294","#c2a5cf","#a6dba0","#008837","#e08214")
mycolors <- c("black", "black", "black", "black", "black", "black", "black","black", "black", "black", "black", "black", "black", "black", "black", "black")
myshapes <- c(21, 22, 24)

### NMDS SITE AND SPECIES PLOT
ggplot(data=all.point.scores, aes(x=NMDS1, y=NMDS2)) + 
  geom_point(data=all.point.scores, aes(x=NMDS1, y=NMDS2, fill = as.factor(linje), color = as.factor(linje), shape= AAR2),  size = 4) + # 
  labs(x = "NMDS1 skår", y= "NMDS2 skår", fill = "Linje-id", color = "Linje-id", shape = "År") + 
  scale_color_manual(values=myfill) +
  scale_fill_manual(values=myfill) +
  scale_shape_manual(values = c(21,22), labels=c("Før","To år etter"),name="Restaurert") +
  theme_bw() +
  theme(axis.title.x = element_text(size=14,hjust=0.5),
        axis.title.y = element_text(size=14,vjust=1),
        axis.text.x = element_text(size=12,color='black'),
        axis.text.y = element_text(size=12,color='black'),
        legend.title = element_text(color="black", size=14),
        legend.text = element_text(color="black", size=12)) +
  theme(panel.grid.minor.x=element_blank(),                          #Hide all the vertical gridlines
        panel.grid.major.x=element_blank(),
        panel.grid.minor.y=element_blank(),                           #Hide all the horizontal gridlines
        panel.grid.major.y=element_blank()) +
  geom_text_repel(aes(label=linje), size=2.5, alpha=0.5, max.overlaps = getOption("ggrepel.max.overlaps", default = 5)) +  #add smaller text
  geom_text_repel(data= species.scores, aes(x=NMDS1, y=NMDS2, label=species), size=3.5, alpha=0.5, max.overlaps = getOption("ggrepel.max.overlaps", default = 15))

##geom_polygon(data=all.point.scores, aes(x = NMDS1, y = NMDS2, fill= linje, group=linje), alpha = 0.30) +

#save as 650x600 // 800x700


myfill <- c("#4a1486","#005a32","#084594","#238b45","#2171b5","#41ab5d","#4292c6","#74c476",
            "#6baed6","#a1d99b","#9ecae1","#c7e9c0","#c6dbef","#e5f5e0","#deebf7","#e08214")