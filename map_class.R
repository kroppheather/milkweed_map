library(caret)
library(randomForest)
library(rgdal)
library(sf)
library(raster)

#### 6-29-2022 ----


Dir1 <- "E:/Google Drive/GIS/drone/campus/2022/out/flight_06_29_2022"


field_pts29 <- st_read("E:/Google Drive/GIS/drone/campus/2022/out/classification/updated_06_29/greenfield.shp")
dead_pts29 <- st_read("E:/Google Drive/GIS/drone/campus/2022/out/classification/updated_06_29/dead.shp")
tree_pts29 <- st_read("E:/Google Drive/GIS/drone/campus/2022/out/classification/updated_06_29/treetubes.shp")
milkweed_pts29 <- st_read("E:/Google Drive/GIS/drone/campus/2022/out/classification/updated_06_29/realmilkweed.shp")

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

F06_29_2022 <- stack(f06_29_2022[[1]], f06_29_2022[[2]],f06_29_2022[[3]],f06_29_2022[[4]],f06_29_2022[[5]],ndvi,VDVI, RGRI)

F06_29 <- mask(F06_29_2022, treeMask, maskvalue=0 )
names(F06_29) <- c("blue","green","red", "red.edge","nir","ndvi","VDVI","RGRI")



field_pts29$class <- rep( "green field", nrow(field_pts29))
milkweed_pts29$class <- rep( "milkweed", nrow(milkweed_pts29))
tree_pts29$class <- rep( "tree tubes", nrow(tree_pts29))
dead_pts29$class <- rep( "inactive field", nrow(dead_pts29))

sampleType <- rep("train", nrow(field_pts29))
set.seed(12)
validSub <- sample(300, 100, replace=FALSE)
sampleType[validSub] <- "valid"
field_pts29$type <- sampleType
milkweed_pts29$type <- sampleType
tree_pts29$type <- sampleType
dead_pts29$type <- sampleType

known_pts <- rbind(field_pts29, milkweed_pts29, tree_pts29, dead_pts29)
known_pts$classID <- ifelse(known_pts$class == "milkweed",1,
                            ifelse(known_pts$class == "green field",2,
                            ifelse(known_pts$class == "tree tubes",3,4)))

known_pts$milkweedID <- ifelse(known_pts$class == "milkweed",1,2)

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
nbands <- 8 #20 bands of information
rf.grid <- expand.grid(mtry=1:round(sqrt(nbands)))

# set random seed for algorithm so you can get the same results when
# running multiple times
set.seed(43)

#note that caret:: will make sure we use train from the caret package
rf_model <- caret::train(x = trainDF[,1:8], #digital number data
                         y = as.factor(trainDF[,12]), #land class we want to predict
                         method = "rf", #use random forest
                         metric="Accuracy", #assess by accuracy
                         trainControl = tc, #use parameter tuning method
                         tuneGrid = rf.grid) #parameter t


#note that caret:: will make sure we use train from the caret package
rf_model2 <- caret::train(x = trainDF[,1:8], #digital number data
                         y = as.factor(trainDF[,13]), #land class we want to predict
                         method = "rf", #use random forest
                         metric="Accuracy", #assess by accuracy
                         trainControl = tc, #use parameter tuning method
                         tuneGrid = rf.grid) #parameter t


#use the model to predict land cover class for the entire raster stack
rf_prediction <- raster::predict(F06_29 , rf_model )
rf_prediction2 <- raster::predict(F06_29 , rf_model2 )



# plot the land cover class (uses LCID number)

plot(rf_prediction, breaks=c(.5,1.5,2.5,3.5,4.5), col= c("plum3","palegreen4", "skyblue3","lightsalmon3"))
plot(rf_prediction2, breaks=c(.5,1.5,2.5), col= c("plum3","palegreen4"))

nnet.grid <- expand.grid(size = seq(from = 10, to = 28, by = 2), # number of neurons units in the hidden layer 
                         decay = seq(from = 0.1, to = 0.6, by = 0.1)) # regularization parameter to avoid over-fitting 

nnet_model <- caret::train(x = trainDF[,c(1:8)], y = as.factor(trainDF[,12]),
                           method = "nnet", metric="Accuracy", trainControl = tc, tuneGrid = nnet.grid,
                           trace=FALSE)

nnet_model2 <- caret::train(x = trainDF[,c(1:8)], y = as.factor(trainDF[,13]),
                           method = "nnet", metric="Accuracy", trainControl = tc, tuneGrid = nnet.grid,
                           trace=FALSE)

nn_prediction <- raster::predict(F06_29 , nnet_model )
nn_prediction2 <- raster::predict(F06_29 , nnet_model2 )

plot(nn_prediction, breaks=c(.5,1.5,2.5,3.5,4.5), col= c("plum3","palegreen4", "skyblue3","lightsalmon3"))
plot(nn_prediction2, breaks=c(.5,1.5,2.5), col= c("plum3","palegreen4"))

validData <- known_pts[known_pts$type == "valid",]
validVecRF1 <- extract(rf_prediction, validData)
validVecRF2 <- extract(rf_prediction2, validData)
validRF1 <- data.frame(prediction=validVecRF1, st_drop_geometry(validData))
validRF2 <- data.frame(prediction=validVecRF2, st_drop_geometry(validData))
validVecNN1 <- extract(nn_prediction, validData)
validVecNN2 <- extract(nn_prediction2, validData)
validNN1 <- data.frame(prediction=validVecNN1, st_drop_geometry(validData))
validNN2 <- data.frame(prediction=validVecNN2, st_drop_geometry(validData))


rf_errorRF1 = confusionMatrix(as.factor(validRF1$prediction),as.factor(validRF1$classID))
rf_errorRF1
rf_errorRF2 = confusionMatrix(as.factor(validRF2$prediction),as.factor(validRF2$milkweedID))
rf_errorRF2
rf_errorNN1 = confusionMatrix(as.factor(validNN1$prediction),as.factor(validNN1$classID))
rf_errorNN1
rf_errorNN2 = confusionMatrix(as.factor(validNN2$prediction),as.factor(validNN2$milkweedID))
rf_errorNN2

milkRC <- function(x){ifelse(x == 1,1,0)}

milkweed_06_29rf <- calc(rf_prediction2, milkRC)
milkweed_06_29nn <- calc(nn_prediction2, milkRC)

writeRaster(milkweed_06_29rf,
            "E:/Google Drive/GIS/drone/campus/2022/out/classification/06_29/milkweed_rf_out.tif",
            format="GTiff")
writeRaster(milkweed_06_29nn,
            "E:/Google Drive/GIS/drone/campus/2022/out/classification/06_29/milkweed_nn_out.tif",
            format="GTiff")

writeRaster(milkweed_06_29,
            "E:/Google Drive/GIS/drone/campus/2022/out/classification/06_29/milkweed_rf_4.tif",
            format="GTiff")

knownp <- st_zm(known_pts, drop=TRUE, what="ZM")


st_write(knownp, "E:/Google Drive/GIS/drone/campus/2022/out/classification/06_29/known_pts.shp")

library(ggplot2)
ggplot(trainDF, aes(x=blue,y=green, color=class))+
  geom_point()

ggplot(trainDF, aes(x=red,y=nir, color=class))+
  geom_point()
ggplot(trainDF, aes(x=red.edge,y=RGRI, color=class))+
  geom_point()

ggplot(trainDF, aes(x=red.edge,y=RGRI, color=class))+
  geom_point()
ggplot(trainDF, aes(x=ndvi,y=VDVI, color=class))+
  geom_point()

ggplot(trainDF, aes(x=ndvi,y=dsm, color=class))+
  geom_point()



pimgs <- stack("K:/Environmental_Studies/hkropp/projects/ksolowey/planet_imagery/files/20220703_145424_16_2428_3B_AnalyticMS_SR_harmonized_clip.tif")

plot(pimgs)
pblue <- pimgs[[1]]* 1.98026572561e-05
plot(pblue)
pgreen <- pimgs[[2]] *2.14775198494e-05
plot(pgreen)
pred <- pimgs[[3]] *2.58511603522e-05
plot(pred)
pnir <- pimgs[[4]] *4.09158868641e-05
plot(pnir)


writeRaster(pblue, "K:/Environmental_Studies/hkropp/projects/ksolowey/planet_imagery/reflectance/blue_reflectance.tif", 
            format="GTiff")
writeRaster(pgreen, "K:/Environmental_Studies/hkropp/projects/ksolowey/planet_imagery/reflectance/green_reflectance.tif", 
            format="GTiff")
writeRaster(pred, "K:/Environmental_Studies/hkropp/projects/ksolowey/planet_imagery/reflectance/red_reflectance.tif", 
            format="GTiff")
writeRaster(pnir, "K:/Environmental_Studies/hkropp/projects/ksolowey/planet_imagery/reflectance/nir_reflectance.tif", 
            format="GTiff")



#### 6-30-2022 ----


Dir2 <- "E:/Google Drive/GIS/drone/campus/2022/out/flight_06_30_2022"


field_pts30 <- st_read("E:/Google Drive/GIS/drone/campus/2022/out/classification/updated_06_30/green_field_06_30_2022.shp")
dead_pts30 <- st_read("E:/Google Drive/GIS/drone/campus/2022/out/classification/updated_06_30/dead_06_30_2022.shp")
milkweed_pts30 <- st_read("E:/Google Drive/GIS/drone/campus/2022/out/classification/updated_06_30/milkweed_06_30_2022.shp")

f06_30_2022 <- stack(paste0(Dir2, "/milkweed_6_30_transparent_reflectance_blue.tif"),
                     paste0(Dir2, "/milkweed_6_30_transparent_reflectance_green.tif"),
                     paste0(Dir2, "/milkweed_6_30_transparent_reflectance_red.tif"),
                     paste0(Dir2, "/milkweed_6_30_transparent_reflectance_red edge.tif"),
                     paste0(Dir2, "/milkweed_6_30_transparent_reflectance_nir.tif"),
                     paste0(Dir2, "/milkweed_6_30_dsm.tif"))

ndvi <- (f06_30_2022[[5]] - f06_30_2022[[3]])/(f06_30_2022[[5]] + f06_30_2022[[3]])
VDVI <- (2*f06_30_2022[[2]] - f06_30_2022[[3]] - f06_30_2022[[1]]) / (2*f06_30_2022[[2]] + f06_30_2022[[3]] + f06_30_2022[[1]])
RGRI <- ( f06_30_2022[[3]]) / (f06_30_2022[[2]] )

plot(ndvi)


minHeight <- cellStats(f06_30_2022[[6]], "min")
height <- f06_30_2022[[6]]-minHeight
plot(height)
treeM <- function(x){
  ifelse(x > 12, 0, 1)
}
treeMask <- calc(height, fun=treeM)
plot(treeMask)
#need to get DEM to make proper height


F06_30_2022 <- stack(f06_30_2022[[1]], f06_30_2022[[2]],f06_30_2022[[3]],f06_30_2022[[4]],f06_30_2022[[5]],ndvi,VDVI, RGRI)

F06_30 <- mask(F06_30_2022, treeMask, maskvalue=0 )
names(F06_30) <- c("blue","green","red", "red.edge","nir","ndvi","VDVI","RGRI")



field_pts30$class <- rep( "green field", nrow(field_pts30))
milkweed_pts30$class <- rep( "milkweed", nrow(milkweed_pts30))
dead_pts30$class <- rep( "inactive field", nrow(dead_pts30))

sampleType <- rep("train", nrow(field_pts30))
set.seed(40)
validSub <- sample(300, 100, replace=FALSE)
sampleType[validSub] <- "valid"
field_pts30$type <- sampleType
milkweed_pts30$type <- sampleType
dead_pts30$type <- sampleType

known_pts <- rbind(field_pts30, milkweed_pts30, dead_pts30)
known_pts$classID <- ifelse(known_pts$class == "milkweed",1,
                            ifelse(known_pts$class == "green field",2,3))

known_pts$milkweedID <- ifelse(known_pts$class == "milkweed",1,2)


#subset point for training and valid

trainData <- known_pts[known_pts$type == "train",]

trainMat <- extract(F06_30, trainData)
trainDF <- data.frame(trainMat, st_drop_geometry(trainData))
trainDF <- na.omit(trainDF)


tc <- trainControl(method = "repeatedcv", # repeated cross-validation of the training data
                   number = 10, # number 10 fold
                   repeats = 10) # number of repeats
###random forests
#Typically square root of number of variables
nbands <- 8 #20 bands of information
rf.grid <- expand.grid(mtry=1:round(sqrt(nbands)))

# set random seed for algorithm so you can get the same results when
# running multiple times
set.seed(45)


#note that caret:: will make sure we use train from the caret package
rf_model2 <- caret::train(x = trainDF[,1:8], #digital number data
                          y = as.factor(trainDF[,13]), #land class we want to predict
                          method = "rf", #use random forest
                          metric="Accuracy", #assess by accuracy
                          trainControl = tc, #use parameter tuning method
                          tuneGrid = rf.grid) #parameter t


#use the model to predict land cover class for the entire raster stack
rf_prediction2 <- raster::predict(F06_30 , rf_model2 )

plot(rf_prediction2, breaks=c(.5,1.5,2.5), col= c("plum3","palegreen4"))

rf_errorRF2 = confusionMatrix(as.factor(validRF2$prediction),as.factor(validRF2$milkweedID))
rf_errorRF2

t.grid <- expand.grid(size = seq(from = 6, to = 28, by = 2), # number of neurons units in the hidden layer 
                      decay = seq(from = 0.1, to = 0.6, by = 0.1)) # regularization parameter to avoid over-fitting 


nnet_model2 <- caret::train(x = trainDF[,c(1:8)], y = as.factor(trainDF[,13]),
                            method = "nnet", metric="Accuracy", trainControl = tc, tuneGrid = nnet.grid,
                            trace=FALSE)


nn_prediction2 <- raster::predict(F06_30 , nnet_model2 )

plot(nn_prediction2, breaks=c(.5,1.5,2.5), col= c("plum3","palegreen4"))


validData <- known_pts[known_pts$type == "valid",]

validVecRF2_30 <- extract(rf_prediction2, validData)

validRF2_30 <- data.frame(prediction=validVecRF2_30, st_drop_geometry(validData))

validVecNN2_30 <- extract(nn_prediction2, validData)

validNN2_30 <- data.frame(prediction=validVecNN2_30, st_drop_geometry(validData))



rf_errorRF2_30 = confusionMatrix(as.factor(validRF2_30$prediction),as.factor(validRF2_30$milkweedID))
rf_errorRF2_30

rf_errorNN2_30 = confusionMatrix(as.factor(validNN2_30$prediction),as.factor(validNN2_30$milkweedID))
rf_errorNN2_30


milkRC <- function(x){ifelse(x == 1,1,0)}

milkweed_06_30rf <- calc(rf_prediction2, milkRC)
milkweed_06_30nn <- calc(nn_prediction2, milkRC)

writeRaster(milkweed_06_30rf,
            "E:/Google Drive/GIS/drone/campus/2022/out/classification/06_30/milkweed_rf_out.tif",
            format="GTiff")
writeRaster(milkweed_06_30nn,
            "E:/Google Drive/GIS/drone/campus/2022/out/classification/06_30/milkweed__06_30_nn_out.tif",
            format="GTiff")
