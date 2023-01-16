fit1 <- lm(NMDS1~ Meter_from_ditch ,data = site.scores)
summary(fit1)
plot(NMDS1~ Meter_from_ditch, data = site.scores)
abline(fit1)
plot(fit1, which = 1)
par(mfrow = c(1, 2)) # This code put two plots in the same window
hist(fit1$residuals) # Histogram of residuals
plot(fit1, which = 2)

fit1.2 <- lm(NMDS2~ Meter_from_ditch+AAR2 ,data = site.scores)
summary(fit1.2)
plot(NMDS2~ Meter_from_ditch+AAR2, data = site.scores)
abline(fit1.2)
plot(fit1.2, which = 1)
par(mfrow = c(1, 2)) # This code put two plots in the same window
hist(fit1.2$residuals) # Histogram of residuals
plot(fit1.2, which = 2)



fit2 <- lm(NMDS2~ Meter_from_ditch ,data = site.scores)
summary(fit2)
plot(NMDS2~ Meter_from_ditch, data = site.scores)
abline(fit2)
plot(fit2, which = 1)
par(mfrow = c(1, 2)) # This code put two plots in the same window
hist(fit2$residuals) # Histogram of residuals
plot(fit2, which = 2)
