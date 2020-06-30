## Import Data ##
datap <- Spatial_Regression_Lat_Long
str(datap)

## Import Library ##
library(shapefiles)
library(maptools)
library(RColorBrewer)

## Import Maps ##
indo<-readShapeSpatial("C:/Users/ACER/Desktop/Matematika/Semester 6/Spasial/Regresi Spasial/spasial/prov.shp")
plot(indo, density=16, col="grey", axes=T, cex.axis=.75)
title(main="Indonesia", sub="Mapped with R",font.sub=2)
title(xlab="Longitude",ylab='Latitude',cex.lab=.75,line=2.25)
text(coordinates(indo), labels=indo@data$NAME_1, cex=.5)

## Merge table
indo@data$row <- as.numeric(row.names(indo@data))
temp <- merge(indo@data, datap, by.x = "NAME_1", by.y = "Province", 
              all.indo = T, sort = F)
indo@data <- temp[order(temp$row),]

## Layout
datap.spdf <- SpatialPointsDataFrame(datap[,9:10], datap)
prov <- list('sp.pointLabel', datap.spdf, label = indo@data$NAME_1, 
             cex = 0.7, col = 'Black')
titik<-list('sp.points', datap.spdf, pch = 19, cex = .8, col = 'Black')

## Variabel
indo@data$Y <- indo@data$`Human Development index`
indo@data$x1 <- indo@data$`Poverty Rate`
indo@data$x2 <- indo@data$`Sanitation Rate`
indo@data$x3 <- indo@data$`Literacy Rate`
indo@data$x4 <- indo@data$`Expected Years of Schooling`
indo@data$x5 <- indo@data$`Per Capita Expenditure`
indo@data$x6 <- indo@data$`Life Expectancy at Birth`

## Plot Human Development Index ##

spplot(indo, "Y", col.regions = brewer.pal(9, "RdBu"), 
       main = "Human Development Index", scales = list(draw = TRUE), 
       sp.layout = list(prov, titik), xlab = "Longitude", 
       ylab = "Latitude", cuts=8)

## Plot Poverty Rate ##

spplot(indo, "x1", col.regions = brewer.pal(9, "BrBG"), 
       main = "Poverty Rate", scales = list(draw = TRUE), 
       sp.layout = list(prov, titik), xlab = "Longitude", 
       ylab = "Latitude", cuts=8)

## Plot Sanitation Rate ##

spplot(indo, "x2", col.regions = brewer.pal(9, "RdYlGn"), 
       main = "Sanitation Rate", scales = list(draw = TRUE), 
       sp.layout = list(prov, titik), xlab = "Longitude", 
       ylab = "Latitude", cuts=8)

## Plot Literacy Rate ##

spplot(indo, "x3", col.regions = brewer.pal(9, "RdYlBu"), 
       main = "Literacy Rate", scales = list(draw = TRUE), 
       sp.layout = list(prov, titik), xlab = "Longitude", 
       ylab = "Latitude", cuts=8)

## Plot Expected Years of Schooling ##

spplot(indo, "x4", col.regions = brewer.pal(9, "PuOr"), 
       main = "Expected Years of Schooling", scales = list(draw = TRUE), 
       sp.layout = list(prov, titik), xlab = "Longitude", 
       ylab = "Latitude", cuts=8)

## Plot Per Capita Expenditure ##

spplot(indo, "x5", col.regions = brewer.pal(9, "PiYG"), 
       main = "Per Capita Expenditure", scales = list(draw = TRUE), 
       sp.layout = list(prov, titik), xlab = "Longitude", 
       ylab = "Latitude", cuts=8)

## Plot Life Expectancy at Birth ##

spplot(indo, "x6", col.regions = brewer.pal(9, "PRGn"), 
       main = "Life Expectancy at Birth", scales = list(draw = TRUE), 
       sp.layout = list(prov, titik), xlab = "Longitude", 
       ylab = "Latitude", cuts=8)

# Brewer Palette
display.brewer.all()