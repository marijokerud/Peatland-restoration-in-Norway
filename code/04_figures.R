library(ggplot2)

ggplot(point.scores, aes(x= Meter_from_ditch, y=NMDS1)) +
  geom_point(shape=1, color= "black", size=2.2) +
  geom_smooth(method='lm', formula= y~x+sqrt(x), se=FALSE, col="black", lwd=1.2) +
  labs(x = "Meters from ditch", y= "NMDS1 scores") +
  theme_bw() +
  annotate(geom = "rect", xmin= 0, xmax= 50, ymin= -0.69642, ymax= 0.1303105, fill="#810f7c", alpha=0.3) +
  #geom_rect(xmin= 0, xmax= 50, ymin= -0.69642, ymax= 0.1303105, fill= "purple") +
  #geom_tile(aes(y= -0.2830547, x= 25, width=50, height= 0.8267306, fill="red")) +
  #scale_fill_gradient2(low = "white", mid = "purple", high = "white", midpoint = -0.2830547) 
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

col= "810f7c" og "8856a7"