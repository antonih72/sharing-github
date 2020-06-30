### Import Data ###
data <- Spatial_Regression_HDI_Indonesia
head(data)
names(data)
summary(data)

### Import Library ###
library(shapefiles)
library(maptools)
library(spdep)
library(sp)
library(Matrix)
library(nortest)


### Data Preparation ###
peta<- readShapeSpatial("C:/Users/ACER/Desktop/Matematika/Semester 6/Spasial/Regresi Spasial/spasial/prov.shp")
plot(peta,axes=T,cex.axis=.75)
title(main="Indonesia", sub="Mapped with R",font.sub=2)
title(xlab="Longitude",ylab='Latitude',cex.lab=.75,line=2.25)
peta.df<- as.data.frame(peta)
head(peta.df)

### Contiguity Neighboor ###

# Queen
peta_nb <- poly2nb(peta,row.names = seq(1,34)) # 34 province
plot(peta, col="white", border="grey")
plot(peta_nb, coordinates(peta), col="red", add=TRUE)
text(coordinates(peta), labels=peta.df$NAME_1, 
     cex=0.7, col="blue",pos=4, offset=0.4)
title(main="Indonesia", sub="Mapped with R",font.sub=2)
title(xlab="Longitude",ylab='Latitude',cex.lab=.75,line=2.25)

# Rook
peta_nb2 <- poly2nb(peta, queen=FALSE, row.names = seq(1,34))
plot(peta, col="white", border="grey")
plot(peta_nb2,coordinates(peta),col="red",cex=0.7,add=TRUE)
text(coordinates(peta), labels=peta.df$NAME_1, 
     cex=0.7, col="blue",pos=4, offset=0.4)
title(main="Indonesia", sub="Mapped with R",font.sub=2)
title(xlab="Longitude",ylab='Latitude',cex.lab=.75,line=2.25)

# Bishop
plot(peta, col="white", border="grey")
plot(diffnb(peta_nb, peta_nb2), coordinates(peta), col="red", add=TRUE)
text(coordinates(peta), labels=peta.df$NAME_1, 
     cex=0.7, col="blue",pos=4, offset=0.4)
title(main="Indonesia", sub="Mapped with R",font.sub=2)
title(xlab="Longitude",ylab='Latitude',cex.lab=.75,line=2.25)

### Wighted Matrix ###
petaw <- nb2listw(peta_nb, zero.policy = TRUE)
petaw2 <- nb2listw(peta_nb2, zero.policy = TRUE)
summary(petaw, zero.policy=TRUE)
?nb2listw

### Moran Test ###

# Human Development Index (Y)
moran.test(data$`Human Development index`, petaw, randomisation = F, 
           alternative = "two.sided", zero.policy = TRUE)
moran.plot(data$`Human Development index`, petaw, col="blue", 
           xlab = "Human Development Index", ylab = "Spatial Lag", 
           zero.policy=TRUE)

# Poverty Rate (x1)
moran.test(data$`Poverty Rate`, petaw, randomisation = F, 
           alternative = "two.sided", zero.policy = TRUE)
moran.plot(data$`Poverty Rate`, petaw, col = "blue", 
           xlab = "Poverty Rate", ylab = "Spatial Lag", zero.policy=TRUE)

# Sanitation Rate (x2)
moran.test(data$`Sanitation Rate`, petaw, randomisation = F, 
           alternative = "two.sided", zero.policy = TRUE)
moran.plot(data$`Sanitation Rate`, petaw, col = "blue", 
           xlab = "Sanitation Rate", ylab = "Spatial Lag", zero.policy=TRUE)

# Literacy Rate (x3)
moran.test(data$`Literacy Rate`, petaw, randomisation = F, 
           alternative = "two.sided", zero.policy = TRUE)
moran.plot(data$`Literacy Rate`, petaw, col = "blue", 
           xlab = "Literacy Rate", ylab = "Spatial Lag", zero.policy=TRUE)

# Expected Years of Schooling (x4)
moran.test(data$`Expected Years of Schooling`, petaw, randomisation = F, 
           alternative = "two.sided", zero.policy = TRUE)
moran.plot(data$`Expected Years of Schooling`, petaw, col = "blue", 
           xlab = "Expected Years of Schooling", ylab = "Spatial Lag", 
           zero.policy = TRUE)

# Per Capita Expenditure (x5)
moran.test(data$`Per Capita Expenditure`, petaw, randomisation = F, 
           alternative = "two.sided", zero.policy = TRUE)
moran.plot(data$`Per Capita Expenditure`, petaw, col = "blue", 
           xlab = "Per Capita Expenditure", ylab = "Spatial Lag", 
           zero.policy = TRUE)

# Life Expectancy at Birth (x6)
moran.test(data$`Life Expectancy at Birth`, petaw, randomisation = F, 
           alternative = "two.sided", zero.policy = TRUE)
moran.plot(data$`Life Expectancy at Birth`, petaw, col = "blue", 
           xlab = "Life Expectancy at Birth", ylab = "Spatial Lag", 
           zero.policy = TRUE)

# Residual
moran.test(res, petaw, randomisation = F, 
           alternative = "two.sided", zero.policy = TRUE)

### Ordinary Least Square ###

Y <- data$`Human Development index`
x1 <- data$`Poverty Rate`
x2 <- data$`Sanitation Rate`
x3 <- data$`Literacy Rate`
x4 <- data$`Expected Years of Schooling`
x5 <- data$`Per Capita Expenditure`
x6 <- data$`Life Expectancy at Birth`

OLS <- lm(Y ~ x1 + x2 + x3 + x4 + x5 + x6)
summary(OLS)

library(MASS)
step.OLS <- stepAIC(OLS, direction="both", trace=FALSE)
summary(step.OLS)

### OLS Assumption Test ###

res <- residuals(OLS)
summary(res)

# residual plot
par(mfrow=c(2,2))
plot(OLS)

## VIF test
library(car)
vif(OLS)

## Normal distribution test
library(nortest)
lillie.test(res)
ad.test(res)
library(stats)
shapiro.test(res)

## Homoscedasticity test Breusch-Pagan
library(lmtest)
bptest(OLS, data = data, studentize="T")

## Autocorrelation of residuals test
library(lmtest)
dwtest(OLS, data = data)

#######################################################################

# Spatial Regression Model

#######################################################################

## SPATIAL AUTOCORRELATION MODEL / GENERAL SPATIAL MODEL (SAC/GSM) ##
SAC <- spatialreg::sacsarlm(step.OLS, data = data, petaw, 
                          zero.policy = TRUE, tol.solve = 1e-20)
summary(SAC)
resSAC <- residuals(SAC)
summary(resSAC)

# Test model
LR.sarlm(SAC, step.OLS)
lillie.test(resSAC)
bptest.sarlm(SAC)
moran.test(resSAC, petaw, zero.policy=TRUE)


## SPATIAL ERROR MODEL (SEM) ##
SEM <- spatialreg::errorsarlm(step.OLS, data = data, petaw, 
                              zero.policy = TRUE, tol.solve = 1e-20)
summary(SEM)
resSEM <- residuals(SEM)
summary(resSEM)

# Test model
LR.sarlm(SEM, step.OLS)
lillie.test(resSEM)
bptest.sarlm(SEM)
moran.test(resSEM, petaw, zero.policy=TRUE)


## SPATIAL AUTOREGRESSIVE LAG MODEL (SAR/SLM) ##
SAR <- spatialreg::lagsarlm(step.OLS, data = data, petaw, 
                            zero.policy = TRUE, tol.solve = 1e-20)
summary(SAR)
resSAR <- residuals(SAR)
summary(resSAR)

# Test model
LR.sarlm(SAR, step.OLS)
lillie.test(resSAR)
bptest.sarlm(SAR)
moran.test(resSAR, petaw, zero.policy=TRUE)


## SPATIAL DURBIN MODEL (SDM) ##
SDM <- spatialreg::lagsarlm(step.OLS, data = data, petaw, type="mixed", 
                            zero.policy = TRUE, tol.solve = 1e-20)
summary(SDM)
resSDM <- residuals(SDM)
summary(resSDM)

# Test model
LR.sarlm(SDM, step.OLS)
lillie.test(resSDM)
bptest.sarlm(SDM)
moran.test(resSDM, petaw, zero.policy=TRUE)


## SPATIAL LAG EXOGENEOUS (SLX) ##
SLX <- spatialreg::lmSLX(step.OLS, data = data, petaw, zero.policy = TRUE)
summary(SLX)
resSLX<-residuals(SLX)
summary(resSLX)

# Test model
LR.sarlm(SLX, step.OLS)
lillie.test(resSLX)
bptest(SLX)
moran.test(resSLX, petaw, zero.policy=TRUE)
AIC(SLX)
logLik(SLX)


## SPATIAL DURBIN ERROR MODEL (SDEM) ##
SDEM <- spatialreg::errorsarlm(step.OLS, data = data, petaw, etype = "emixed", 
                               zero.policy = TRUE, tol.solve = 1e-20)
summary(SDEM)
resSDEM<-residuals(SDEM)
summary(resSDEM)

# Test model
LR.sarlm(SDEM, step.OLS)
lillie.test(resSDEM)
bptest.sarlm(SDEM)
moran.test(resSDEM, petaw,zero.policy = TRUE)


## GENERAL NESTED SPATIAL MODEL (GNSM) ##
GNSM <- spatialreg::sacsarlm(step.OLS, data = data, petaw, type = "sacmixed",
                             zero.policy = TRUE, tol.solve = 1e-20)
summary(GNSM)
resGNSM<- residuals(GNSM)
summary(resGNSM)

# Test model
LR.sarlm(GNSM, step.OLS)
lillie.test(resGNSM)
bptest.sarlm(GNSM)
moran.test(resGNSM, petaw, zero.policy=TRUE)


### Spatial Model Comparison ###

## Based on AIC
AICs <- c(AIC(step.OLS), AIC(SAC), AIC(SEM), AIC(SAR), AIC(SDM), AIC(SLX), 
          AIC(SDEM), AIC(GNSM))
plot(AICs, lwd = 1.5, xaxt = "n", xlab = "", col = "red")
axis(1, at = 1:8, labels = F) #8= number of models
labels <- c("OLS", "GSM", "SEM", "SLM", "SDM", "SLX", "SDEM", "GNSM")
text(1:8, par("usr")[3]-.75, srt = 50, adj = 1, labels = labels, xpd = T)
mtext(side = 1, text = "Model Specification", line = 3)

## Based on Logloikelihood
Loglik <- c(logLik(step.OLS), logLik(SAC), logLik(SEM), 
            logLik(SAR), logLik(SDM), logLik(SLX), 
            logLik(SDEM), logLik(GNSM))
plot(Loglik, lwd = 1.5, xaxt = "n", xlab = "", col = "red")
axis(1, at = 1:8, labels = F) #8= number of models
labels <- c("OLS", "GSM", "SEM", "SLM", "SDM", "SLX", "SDEM", "GNSM")
text(1:8, par("usr")[3]-.75, srt = 50, adj = 1, labels = labels, xpd = T)
mtext(side = 1, text = "Model Specification", line = 3)
