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
  ifelse(x > 12, 0, 1)
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
trainDF <- na.omit(trainDF)


tc <- trainControl(method = "repeatedcv", # repeated cross-validation of the training data
                   number = 10, # number 10 fold
                   repeats = 10) # number of repeats
###random forests
#Typically square root of number of variables
nbands <- 9 #20 bands of information
rf.grid <- expand.grid(mtry=1:round(sqrt(nbands)))

# set random seed for algorithm so you can get the same results when
# running multiple times
set.seed(43)

#note that caret:: will make sure we use train from the caret package
rf_model <- caret::train(x = trainDF[,1:9], #digital number data
                         y = as.factor(trainDF[,13]), #land class we want to predict
                         method = "rf", #use random forest
                         metric="Accuracy", #assess by accuracy
                         trainControl = tc, #use parameter tuning method
                         tuneGrid = rf.grid) #parameter t


#use the model to predict land cover class for the entire raster stack
rf_prediction <- raster::predict(F06_29 , rf_model )
# plot the land cover class (uses LCID number)

plot(rf_prediction, breaks=c(.5,1.5,2.5), col= c("purple","grey"))


validData <- known_pts[known_pts$type == "valid",]
validVec <- extract(rf_prediction, validData)

validDF <- data.frame(prediction=validVec, st_drop_geometry(trainData))


rf_errorM = confusionMatrix(as.factor(validDF$prediction),as.factor(validDF$classID))

milkRC <- function(x){ifelse(x == 1,1,0)}

milkweed_06_29 <- calc(rf_prediction, milkRC)

writeRaster(milkweed_06_29,
            "E:/Google Drive/GIS/drone/campus/2022/out/classification/06_29/milkweed_rf_2.tif",
            format="GTiff")
