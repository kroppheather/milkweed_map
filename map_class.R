library(caret)
library(randomForest)
library(rgdal)
library(sf)
library(raster)

Dir1 <- "E:/Google Drive/GIS/drone/campus/2022/out/flight_06_29_2022"


field_pts29 <- st_read("E:/Google Drive/GIS/drone/campus/2022/out/valid_pts/06_29_2022/field_06_29_2022.shp")

milkweed_pts29 <- st_read("E:/Google Drive/GIS/drone/campus/2022/out/valid_pts/06_29_2022/milkweed_06_29_2022.shp")

f06_29_2022 <- stack(paste0(Dir1, "/flight_t3_06_29_2019_transparent_reflectance_blue.tif"),
                     paste0(Dir1, "/flight_t3_06_29_2019_transparent_reflectance_green.tif"),
                     paste0(Dir1, "/flight_t3_06_29_2019_transparent_reflectance_red.tif"),
                     paste0(Dir1, "/flight_t3_06_29_2019_transparent_reflectance_red edge.tif"),
                     paste0(Dir1, "/flight_t3_06_29_2019_transparent_reflectance_nir.tif"),
                    paste0(Dir1, "/flight_t3_06_29_2019_dsm.tif"))

ndvi <- (f06_29_2022[[5]] - f06_29_2022[[3]])/(f06_29_2022[[5]] + f06_29_2022[[3]])
VDVI <- (2*f06_29_2022[[2]] - f06_29_2022[[3]] - f06_29_2022[[1]]) / (2*f06_29_2022[[2]] + f06_29_2022[[3]] + f06_29_2022[[1]])
RGRI <- ( f06_29_2022[[3]]) / (f06_29_2022[[2]] )

minHeight <- cellStats(f06_29_2022[[6]], "min")
height <- f06_29_2022[[6]]-minHeight

treeM <- function(x){
  ifelse(x > 11, 0, 1)
}
treeMask <- calc(height, fun=treeM)

F06_29_2022 <- stack(f06_29_2022, ndvi,VDVI, RGRI)

F06_29 <- mask(F06_29_2022, treeMask, maskvalue=0 )
names(F06_29) <- c("blue","green","red", "red.edge","nir","dsm","ndvi","VDVI","RGRI")

field_pts29$class <- base::rep( "field", nrow(field_pts29))
milkweed_pts29$class <- base::rep( "milkweed", nrow(milkweed_pts29))

sampleType <- rep("train", nrow(field_pts29))
set.seed(12)
validSub <- sample(200, 100, replace=FALSE)
sampleType[validSub] <- "valid"
field_pts29$type <- sampleType
milkweed_pts29$type <- sampleType

known_pts <- rbind(field_pts29, milkweed_pts29)
known_pts$classID <- ifelse(known_pts$class == "milkweed",1,2)

plot(F06_29[[7]])
plot(known_pts$geometry, add=TRUE)


#subset point for training and valid

trainData <- known_pts[known_pts$type == "train",]

trainMat <- extract(F06_29, trainData)
trainDF <- data.frame(trainMat, st_drop_geometry(trainData))

