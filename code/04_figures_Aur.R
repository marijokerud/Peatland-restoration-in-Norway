library(ggplot2)
library(ggrepel)
#library(wesanderson)


#Referanselinje confidens interval
R5 <- all.point.scores %>% 
  slice(62:76)
modelR<- lm(NMDS1~1, R5)
confint(modelR, level = 0.95)

R5 %>% summarise(avg = mean(NMDS1))

#KALDVASSMYRA RAPPORT
### POLYNOMIAL REGRESSION PLOT
ggplot(point.scores, aes(x= Meter_from_ditch, y=NMDS1)) +
  geom_point(shape=1, color= "black", size=2.2) +
  geom_smooth(method='lm', formula= y~x+sqrt(x), col="black", fill="#008837", lwd=1.2) + #Remove confidence interval se=FALSE,
  geom_hline(aes(yintercept = 0.05665973), linetype = "dotted", col = "#810f7c") +
  labs(x = "Avstand fra grøft (m)", y= "NMDS1 skår") +
  theme_bw() +
  annotate(geom = "rect", xmin= 0, xmax= 50, ymin= -0.05283458, ymax= 0.166857, fill="#810f7c", alpha=0.3) +
  theme(axis.title.x = element_text(size=14,hjust=0.5),
        axis.title.y = element_text(size=14,vjust=1),
        axis.text.x = element_text(size=12,color='black'),
        axis.text.y = element_text(size=12,color='black'),
        legend.title = element_text(color="black", size=14),
        legend.text = element_text(color="black", size=12)) +
  theme(panel.grid.minor.x=element_blank(),                          #Hide all the vertical gridlines
        panel.grid.major.x=element_blank(),
        #panel.grid.minor.y=element_blank(),                           #Hide all the horizontal gridlines
        panel.grid.major.y=element_blank()) 

#save as 575x650

#col= "810f7c" og "8856a7"

myfill <- c("#7b3294","#008837","#c2a5cf","#a6dba0","#e7d4e8","#e08214")

### NMDS SITE AND SPECIES PLOT
ggplot(data=all.point.scores, aes(x=NMDS1, y=NMDS2)) + 
  geom_point(data=all.point.scores, aes(x=NMDS1, y=NMDS2, fill = as.factor(Meter_from_ditch), color = as.factor(Meter_from_ditch), shape= AAR2),  size = 4) + # 
  labs(x = "NMDS1 skår", y= "NMDS2 skår", fill = "Avstand fra grøft (m)", color = "Avstand fra grøft (m)", shape = "Year") +
  scale_color_manual(values=myfill) +
  scale_fill_manual(values=myfill) +
  scale_shape_manual(values = c(21,22,24), labels=c("Ett år før","To år etter","Sju år etter"),name="Restaurert") +
  #guides(fill = FALSE) +
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
  #geom_text_repel(aes(label=linje), size=2.5, alpha=0.5, max.overlaps = getOption("ggrepel.max.overlaps", default = 5)) +  #add smaller text
  geom_text_repel(data= species.scores, aes(x=NMDS1, y=NMDS2, label=species), size=3.5, alpha=0.5, max.overlaps = getOption("ggrepel.max.overlaps", default = 15))

##geom_polygon(data=all.point.scores, aes(x = NMDS1, y = NMDS2, fill= linje, group=linje), alpha = 0.30) +

#save as 650x600 // 800x700



myfill <- c("#4a1486","#005a32","#084594","#238b45","#2171b5","#41ab5d","#4292c6","#74c476",
            "#6baed6","#a1d99b","#9ecae1","#c7e9c0","#c6dbef","#e5f5e0","#deebf7","#e08214")