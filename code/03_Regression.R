library(ggplot2)

fit1.1 <- lm(NMDS1~ Meter_from_ditch + I(sqrt(Meter_from_ditch)), data = point.scores)
summary(fit1.1)
par(mfrow = c(3, 2)) # This code put two plots in the same window
plot(NMDS1~ AAR+ Meter_from_ditch ,data = point.scores)
#abline(fit1.1)
plot(fit1.1, which = 1)
hist(fit1.1$residuals) # Histogram of residuals
plot(fit1.1, which = 2)

 
  ggplot(point.scores, aes(x= Meter_from_ditch, y=NMDS1)) +
  geom_point(shape=1, color= "black") +
  geom_smooth(method='lm', formula= y~x+sqrt(x), se=FALSE, col="black", lwd=1.2) +
  labs(x = "Meters from ditch", y= "NMDS1 scores") +
  theme_bw() +
  annotate(geom = "rect", xmin= 0, xmax= 50, ymin= -0.69642, ymax= 0.1303105, fill="grey", alpha=0.5) +
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

 


### JOACHIM metode
fitxxxx2 <- lm(NMDS1~ Meter_from_ditch + I(Meter_from_ditch^(1/2)), data = point.scores)
summary(fitxxxx2)

fitxxxx3 <- lm(NMDS1~ Meter_from_ditch + I(Meter_from_ditch^2), data = point.scores)
summary(fitxxxx3)

par(mfrow = c(1,1))
plot(NMDS1~Meter_from_ditch ,data = point.scores)
points(0:50,predict(fitxxxx2,newdata=data.frame(Meter_from_ditch=(0:50))),type='l',col="blue")
points(0:50,predict(fitxxxx3,newdata=data.frame(Meter_from_ditch=(0:50))),type='l',col="purple")


fit1.2 <- lm(NMDS1~ AAR* Meter_from_ditch + I(sqrt(Meter_from_ditch)), data = point.scores)
summary(fit1.2)
par(mfrow = c(3, 2)) # This code put two plots in the same window
plot(NMDS1~ AAR+ Meter_from_ditch ,data = point.scores)
#abline(fit1.2)
plot(fit1.2, which = 1)
hist(fit1.2$residuals) # Histogram of residuals
plot(fit1.2, which = 2)


fit2.1 <- lm(NMDS2~ AAR+ Meter_from_ditch + I(sqrt(Meter_from_ditch)), data = point.scores)
summary(fit2.1)
par(mfrow = c(3, 2)) # This code put two plots in the same window
plot(NMDS2~ AAR+ Meter_from_ditch, data = point.scores)
#abline(fit2.1)
plot(fit2.1, which = 1)
hist(fit2.1$residuals) # Histogram of residuals
plot(fit2.1, which = 2)


fit2.2 <- lm(NMDS2~ AAR*Meter_from_ditch + I(sqrt(Meter_from_ditch)), data = point.scores)
summary(fit2.2)
par(mfrow = c(3, 2)) # This code put two plots in the same window
plot(NMDS2~ AAR*Meter_from_ditch ,data = point.scores)
#abline(fit2.2)
plot(fit2.2, which = 1)
hist(fit2.2$residuals) # Histogram of residuals
plot(fit2.2, which = 2)
