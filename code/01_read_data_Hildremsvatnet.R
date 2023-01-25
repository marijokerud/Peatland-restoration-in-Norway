#LOAD DATA
library(readxl)
library(tidyverse)
library(labdsv)
library(ggplot2)

artslinjerH <- read_excel(path = "data/Data_Hildremsvatnet.xlsx", sheet = "Hildremsvatnet", col_names = TRUE)
fungroupH <- read_excel(path = "data/Data_Hildremsvatnet.xlsx", sheet = "Sheet1", col_names = TRUE)

artslinjerH <- artslinjerH %>% 
  left_join(fungroupH, by="Art")

freqH <- artslinjerH %>% 
  group_by(AAR1, Artslinje_id, Funk_gruppe) %>% 
  summarise(n = n()) %>% 
  mutate(freq = n / sum(n)) %>% 
  mutate(AAR1 =as.character(AAR1))

mycolors <- c("#a6dba0", "#7b3294", "#c2a5cf", "#008837", "#f7f7f7")

ggplot() +
  geom_col(data = freqH, aes(x= Funk_gruppe, y= freq, fill = AAR1), position = "dodge") +
  labs(x = "Functional group", y= "Frequency", fill = "") +
  #scale_fill_manual(values=c("#c2a5cf", "#a6dba0")) +
  scale_fill_manual(values=c("#7fbf7b", "#af8dc3")) +
  theme_bw() +
  theme(axis.title.x = element_text(size=14,hjust=0.5),
        axis.title.y = element_text(size=14,vjust=1),
        axis.text.x = element_text(size=12,color='black'),
        axis.text.y = element_text(size=12,color='black'),
        legend.title = element_text(color="black", size=14),
        legend.text = element_text(color="black", size=12)) +
  theme(panel.grid.minor.x=element_blank(),                          #Hide all the vertical gridlines
        panel.grid.major.x=element_blank())

        #panel.grid.minor.y=element_blank(),                           #Hide all the horizontal gridlines
        #panel.grid.major.y=element_blank()) 
##save as 
        