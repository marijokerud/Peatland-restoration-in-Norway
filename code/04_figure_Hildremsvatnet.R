#LOAD DATA
library(readxl)
library(tidyverse)
library(labdsv)
library(ggplot2)

myfill <- c("#d8b365","#5ab4ac")

### NMDS SITE AND SPECIES PLOT
ggplot(data=all.point.scores, aes(x=NMDS1, y=NMDS2)) + 
  geom_point(data=all.point.scores, aes(x=NMDS1, y=NMDS2, color = AAR2, fill = AAR2, shape = linje), size = 4) +  
  labs(x = "NMDS1 scores", y= "NMDS2 scores", fill = "Year", color = "Year", shape = "Species line") + #,
  scale_color_manual(values=myfill, labels=c("Before","Two years after"),name="Restoration") +
  scale_fill_manual(values=myfill, labels=c("Before","Two years after"),name="Restoration") +
  scale_shape_manual(values = c(21,22,23,24)) +
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