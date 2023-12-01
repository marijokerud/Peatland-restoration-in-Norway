library(ggplot2)
library(ggrepel)
#library(wesanderson)

#Referance line confidence interval
R5 <- all.point.scores %>% 
  slice(70:74) 
modelR<- lm(NMDS1~1, R5)
confint(modelR, level = 0.95)

R5 %>% summarise(avg = mean(NMDS1))

#KALDVASSMYRA
### POLYNOMIAL REGRESSION PLOT
ggplot(all.point.scores, aes(x= Meter_from_ditch, y=NMDS1)) +
  geom_point(shape=1, color= "black", size=2.2) +
  geom_smooth(method='lm', formula= y~x+sqrt(x), se=FALSE, col="black", lwd=1.2) +
  geom_hline(aes(yintercept = -0.3732912), linetype = "dotted", col = "#e08214") +
  labs(x = "Meters from ditch", y= "NMDS1 scores") +
  theme_bw() +
  annotate(geom = "rect", xmin= 0, xmax= 51, ymin= -0.9631129, ymax= 0.2165305, fill="#e08214", alpha=0.4) +
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

### NMDS SITE AND SPECIES PLOT
#DISTANCE from ditch
myfill <- c("#762a83","#1b7837","#9970ab","#5aae61","#c2a5cf","#a6dba0","#e08214")
distance<- 
  ggplot(data=all.point.scores, aes(x=NMDS1, y=NMDS2)) + 
  geom_point(data=all.point.scores, aes(x=NMDS1, y=NMDS2, fill = as.factor(Meter_from_ditch), color = as.factor(Meter_from_ditch)),  size = 4) + # 
  labs(x = "NMDS1 scores", y= "NMDS2 scores", fill = "Distance from ditch", color = "Distance from ditch") + #,
  scale_color_manual(values=myfill) +
  scale_fill_manual(values=myfill) +
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
  geom_text_repel(data= species.scores, aes(x=NMDS1, y=NMDS2, label=species), size=3.5, alpha=0.5, max.overlaps = getOption("ggrepel.max.overlaps", default = 15))
#save as 650x600 // 800x700

#YEAR
myfill <- c("#998ec3","#d8b365","#5ab4ac")
year<- 
  ggplot(data=all.point.scores, aes(x=NMDS1, y=NMDS2)) + 
  geom_point(data=all.point.scores, aes(x=NMDS1, y=NMDS2, fill = AAR, color = AAR, shape= AAR),  size = 4) + # 
  labs(x = "NMDS1 scores", y= "NMDS2 scores", fill = "Year", color = "Year", shape = "Year") + #,
  scale_color_manual(values=myfill, labels=c("","", ""),name="") +
  scale_fill_manual(values=myfill, labels=c("","", ""),name="") +
  scale_shape_manual(values = c(21,22,24), labels=c("Before","Two years after", "Five years after"),name="Restoration") +
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
  geom_text_repel(data= species.scores, aes(x=NMDS1, y=NMDS2, label=species), size=3.5, alpha=0.5, max.overlaps = getOption("ggrepel.max.overlaps", default = 15))
#save as 650x600 // 800x700


library(cowplot)

ggdraw() +
draw_plot(distance, 0, 0, .5) +
draw_plot(year, .5, 0, .5) +
draw_plot_label(c("(a)", "(b)"), c(0, .5), c(1, 1), hjust = -0.2,  size = 15)
#save as 1400x700


### ONE PLOT
ggplot(data=all.point.scores, aes(x=NMDS1, y=NMDS2)) + 
  geom_point(data=all.point.scores, aes(x=NMDS1, y=NMDS2, fill = as.factor(Meter_from_ditch), color = as.factor(Meter_from_ditch), shape= AAR2),  size = 4) + # 
  labs(x = "NMDS1 scores", y= "NMDS2 scores", fill = "Distance from ditch", color = "Distance from ditch", shape = "Year") + #,
  scale_color_manual(values=myfill) +
  scale_fill_manual(values=myfill) +
  scale_shape_manual(values = c(21,22,24), labels=c("Before","Two years after", "Five years after"),name="Restoration") +
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
  geom_text_repel(data= species.scores, aes(x=NMDS1, y=NMDS2, label=species), size=3.5, alpha=0.5, max.overlaps = getOption("ggrepel.max.overlaps", default = 15))


#save as 650x600 // 800x700

### LEFT OVER CODE
#geom_text_repel(aes(label=linje), size=2.5, alpha=0.5, max.overlaps = getOption("ggrepel.max.overlaps", default = 5)) +  #add smaller text
#geom_polygon(data=all.point.scores, aes(x = NMDS1, y = NMDS2, fill= linje, group=linje), alpha = 0.30) +


