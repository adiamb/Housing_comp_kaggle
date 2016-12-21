require(data.table)
require(readr)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
require(plyr)
require(dplyr)
require(ggplot2)
require(mice)
home_train = fread('~/Downloads/Houisng_kaggle_Dec19/train_house.csv', header = T, sep = ";", stringsAsFactors = T)
home_test = fread('~/Downloads/Houisng_kaggle_Dec19/test.csv', stringsAsFactors = T)
total = rbind.data.frame(home_train, home_test, fill = T)

total$OverallCond = as.factor(total$OverallCond)
total$OverallQual= as.factor(total$OverallQual)
#total$YearBuilt = as.character(as.numeric(total$YearBuilt))
#total$YearRemodAdd = as.character(as.numeric(total$YearRemodAdd))
total$YrSold = as.factor(total$YrSold)
total$BsmtHalfBath = as.factor(total$BsmtHalfBath)
total$BsmtFullBath = as.factor(total$BsmtFullBath)
total$FullBath = as.factor(total$FullBath)
total$HalfBath = as.factor(total$HalfBath)
total$BedroomAbvGr = as.factor(total$BedroomAbvGr)
total$KitchenAbvGr = as.factor(total$KitchenAbvGr)
total$TotRmsAbvGrd = as.factor(total$TotRmsAbvGrd)
total$MoSold = as.factor(total$MoSold)
total$year_feature = ifelse(total$YearBuilt <= 1900, 1,
                            ifelse(total$YearBuilt >= 1901 & total$YearBuilt <= 1930, 2,
                                   ifelse(total$YearBuilt >= 1931 & total$YearBuilt <= 1960, 3,
                                          ifelse(total$YearBuilt >= 1961 & total$YearBuilt <= 1990, 4,
                                                 5))))


total$year_feature2 = ifelse(total$YearRemodAdd <= 1950, 1,
                             ifelse(total$YearRemodAdd >= 1951 & total$YearRemodAdd <= 2000, 2,
                                    3))


total$year_feature = as.factor(total$year_feature)
total$year_feature2 = as.factor(total$year_feature2)


###imput missing features and rerun the model


x = ('year_feature2+BsmtFullBath+year_feature+GrLivArea+MSSubClass+OverallQual+Neighborhood+LotArea+OverallCond+BsmtFinSF1+GarageCars+BsmtQual')
x
strsplit(x, "+")
strsplit(x, "\\+")
names1=strsplit(x, "\\+")
names1
names1 = as.vector(names1)
names1
length(names1)
names1=as.vector(strsplit(x, "\\+"))
names1
names1=names1[[1]]
names1

train1=train_home[colnames(train_home) %in% names1]
apply(train1, 2, FUN = function(x) (sum(is.na(x))))
train1$BsmtQual
mice(train1, m = 1)
train_mice=mice(train1, m = 1)
train_mice$imp$BsmtQual$`1`
train1$BsmtQual[is.na(train1$BsmtQual)] = train_mice$imp$BsmtQual$`1`
test_home = as_data_frame(test_home)
test1 = test_home[colnames(test_home) %in% names1]
apply(test1, 2, FUN = function(x) (sum(is.na(x))))
test_mice=mice(test1, m=1, method = "cart")
test1$BsmtQual[is.na(test1$BsmtQual)]=test_mice$imp$BsmtQual$`1`
test1$BsmtFinSF1[is.na(test1$BsmtFinSF1)]=test_mice$imp$BsmtFinSF1$`1`
test1$BsmtFullBath[is.na(test1$BsmtFullBath)]=test_mice$imp$BsmtFullBath$`1`
test1$GarageCars[is.na(test1$GarageCars)]=test_mice$imp$GarageCars$`1`

train1 = cbind.data.frame(train_home$SalePrice, train1)
names(train1)[1]=c("SalePrice")
model1 = glm(SalePrice ~ year_feature2+BsmtFullBath+year_feature+LotArea*GrLivArea+MSSubClass+OverallQual+Neighborhood+BsmtFinSF1+GarageCars+BsmtQual, data = train1)
summary(model1)
sales=predict(model1, test1)
sub=cbind.data.frame(test_home$Id, sales)
colnames(sub) = c("Id", "SalePrice")
write_csv(sub, path = '~/Desktop/sub_housing_19dec_144pm.csv')



####model2#####################
total = as_data_frame(total)
total2=total[-81]
apply(total2, 2, FUN = function(x)(sum(is.na(x))))
total2$PoolQC = NULL
total2$Alley = NULL
total2$Fence = NULL
total2$MiscFeature = NULL
apply(total2, 2, FUN = function(x)(sum(is.na(x)))) %>% sort()
total2$FireplaceQu = NULL
total_mice=mice(total2, m=1, method = "cart")
total$MSZoning[is.na(total$MSZoning)] = total_mice$imp$MSZoning$`1`
total$LotFrontage[is.na(total$LotFrontage)] = total_mice$imp$LotFrontage$`1`
total$Utilities[is.na(total$Utilities)] = total_mice$imp$Utilities$`1`
total$Exterior1st[is.na(total$Exterior1st)] = total_mice$imp$Exterior1st$`1`
total$Exterior2nd[is.na(total$Exterior2nd)] = total_mice$imp$Exterior2nd$`1`
total$MasVnrType[is.na(total$MasVnrType)] = total_mice$imp$MasVnrType$`1`
total$MasVnrArea[is.na(total$MasVnrArea)] = total_mice$imp$MasVnrArea$`1`
total$BsmtQual[is.na(total$BsmtQual)] = total_mice$imp$BsmtQual$`1`
total$BsmtCond[is.na(total$BsmtCond)] = total_mice$imp$BsmtCond$`1`
total$BsmtExposure[is.na(total$BsmtExposure)] = total_mice$imp$BsmtExposure$`1`
total$BsmtFinType1[is.na(total$BsmtFinType1)] = total_mice$imp$BsmtFinType1$`1`
total$BsmtFinType2[is.na(total$BsmtFinType2)] = total_mice$imp$BsmtFinType2$`1`
total$BsmtFinSF1[is.na(total$BsmtFinSF1)] = total_mice$imp$BsmtFinSF1$`1`
total$BsmtFinSF2[is.na(total$BsmtFinSF2)] = total_mice$imp$BsmtFinSF2$`1`
total$BsmtUnfSF[is.na(total$BsmtUnfSF)] = total_mice$imp$BsmtUnfSF$`1`
total$TotalBsmtSF[is.na(total$TotalBsmtSF)] = total_mice$imp$TotalBsmtSF$`1`
total$BsmtFullBath[is.na(total$BsmtFullBath)] = total_mice$imp$BsmtFullBath$`1`
total$BsmtHalfBath[is.na(total$BsmtHalfBath)] = total_mice$imp$BsmtHalfBath$`1`
total$Electrical[is.na(total$Electrical)] = total_mice$imp$Electrical$`1`
total$Functional[is.na(total$Functional)] = total_mice$imp$Functional$`1`
total$GarageType[is.na(total$GarageType)] = total_mice$imp$GarageType$`1`
total$GarageYrBlt[is.na(total$GarageYrBlt)] = total_mice$imp$GarageYrBlt$`1`
total$GarageFinish[is.na(total$GarageFinish)] = total_mice$imp$GarageFinish$`1`
total$GarageCars[is.na(total$GarageCars)] = total_mice$imp$GarageCars$`1`
total$GarageArea[is.na(total$GarageArea)] = total_mice$imp$GarageArea$`1`
total$GarageQual[is.na(total$GarageQual)] = total_mice$imp$GarageQual$`1`
total$GarageCond[is.na(total$GarageCond)] = total_mice$imp$GarageCond$`1`
total$SaleType[is.na(total$SaleType)] =total_mice$imp$SaleType$`1`
total$KitchenQual[is.na(total$KitchenQual)] = total_mice$imp$KitchenQual$`1`
apply(total, 2, FUN = function(x) (sum(is.na(x))))
###remove columsn that have many NAS
total$PoolQC = NULL
total$Alley = NULL
total$Fence = NULL
total$MiscFeature = NULL
total$FireplaceQu = NULL
str(total)

##do some feature enigneering 
total$MSZoning = ifelse(total$MSZoning == "RL", 1, 2)
total$LotShape = ifelse(total$LotShape == "Reg", 1, 2)
total$LandContour = ifelse(total$LandContour == "Lvl", 1, 2)
total$LotConfig = ifelse(total$LotConfig == "Inside", 1, 2)
total$LandSlope = ifelse(total$LandSlope == "Gtl", 1, 2)
total$Condition1 = ifelse(total$Condition1 == "Norm", 1, 2)
total$Condition2 = ifelse(total$Condition2 == "Norm", 1, 2)
total$HouseStyle = ifelse(total$HouseStyle == "1Story", 1,
                          ifelse(total$HouseStyle == "2Story", 2,
                                 3))

total$RoofMatl = ifelse(total$RoofMatl == "CompShg", 1, 2)
total$RoofStyle = ifelse(total$RoofStyle == "Gable", 1,
                         ifelse(total$RoofStyle == "Hip", 2,
                                3))

total$Exterior1st = as.character(total$Exterior1st)
total$Exterior2nd = as.character(total$Exterior2nd)
total$Exterior1st=if_else(total$Exterior1st %in% Ext1, "other", total$Exterior1st)
total$Exterior2nd=if_else(total$Exterior2nd %in% Ext2, "other", total$Exterior2nd)


#assign factors to gargaedate
total$year_garage2 = ifelse(total$GarageYrBlt <= 1950, 1,
                            ifelse(total$GarageYrBlt >= 1951 & total$GarageYrBlt <= 2000, 2,
                                   3))
##asgin remod year
total$remodel_feature = ifelse(total$YearRemodAdd == total$YrSold, 1, 2)
total$year_garage2 = as.factor(total$year_garage2)
total$year_feature = as.factor(total$year_feature)
total$year_feature2 = as.factor(total$year_feature2)
total$OverallQual = as.factor(total$OverallQual)
total$OverallCond = as.factor(total$OverallCond)
total$LotConfig = as.factor(total$LotConfig)
total$Condition1 = as.factor(total$Condition1)
total$Condition2 = as.factor(total$Condition2)
total$RoofMatl = as.factor(total$RoofMatl)
total$MSSubClass = as.factor(total$MSSubClass)
total$RoofStyle = as.factor(total$RoofStyle)
colnames(total)[43:44]=c("floor1_SF", "floor2_SF")
colnames(total)[68] =c("Porch_3ssn")
##split into test and train
train_home = total[1:1460,]
test_home = total[1461:nrow(total),]

mod1=glm(SalePrice~GrLivArea+remodel_feature+LotArea+BsmtFullBath+FullBath+GarageCars+GarageType+Neighborhood+OverallQual+BsmtExposure+BsmtFinSF1+BsmtFinSF2+KitchenQual+ScreenPorch+year_feature+year_feature2+year_garage2, data = train_home)
summary(mod1)
sales=predict(mod1, test_home)
sub=cbind.data.frame(test_home$Id, sales)
colnames(sub) = c("Id", "SalePrice")
write_csv(sub, path = '~/Desktop/sub_housing_20dec_3.csv')

mod2=glm(SalePrice~RoofMatl+floor1_SF+floor2_SF+Condition2+Neighborhood+LotArea+BsmtFinSF1+OverallQual+year_feature+GrLivArea+MSSubClass+GarageCars+BsmtQual, data = train_home)
sales=predict(mod2, test_home)
sub=cbind.data.frame(test_home$Id, sales)
colnames(sub) = c("Id", "SalePrice")
write_csv(sub, path = '~/Desktop/sub_housing_19dec_423pm.csv')


require(randomForest)
rf_model <- randomForest(SalePrice~.-Id, data = train_home, ntree=800)
predict(rf_model, test_home)
sales=predict(rf_model, test_home)
sub=cbind.data.frame(test_home$Id, sales)
colnames(sub) = c("Id", "SalePrice")
write_csv(sub, path = '~/Desktop/sub_housing_19dec_443pm.csv')



library(caret)
set.seed(998)
train_home2=train_home[-c(1, 19, 20, 73)]
test_home2 =test_home[-c(1, 19, 20, 73, 76)]
test_mm2 = model.matrix(~., test_home2)
mm1= model.matrix(~., train_home2)
price = train_home$SalePrice/1000
glmmodel = cv.glmnet(x=mm1, y=price, family = "gaussian", type.measure = "deviance", alpha = 0.6, nfolds = 10)
plot(glmmodel)
glmmodel$lambda
glmmodel$lambda.min
glmmodel$glmnet.fit$beta[which(glmmodel$glmnet.fit$beta[,51]!=0),51] %>% sort()
sales=predict.cv.glmnet(glmmodel, test_mm2)
sub=cbind.data.frame(test_home$Id, sales*1000)
colnames(sub) = c("Id", "SalePrice")
write_csv(sub, path = '~/Desktop/sub_housing_20dec_glmnet.csv')

inTraining <- createDataPartition(train_home2$SalePrice, p = .75, list = FALSE)
training <- train_home2[ inTraining,]
testing  <-train_home2[-inTraining,]
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

set.seed(825)
gbmFit1 <- train(SalePrice/1000 ~., data = training, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = T)



gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

nrow(gbmGrid)

set.seed(825)
gbmFit2 <- train(SalePrice ~ ., data = training, 
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = T, 
                 ## Now specify the exact models 
                 ## to evaluate:
                 tuneGrid = gbmGrid)
gbmFit2 <- train(SalePrice ~ ., data = training, 
                 method = "gbm", 
                 trControl = fitControl, 
                 verbose = T, 
                 ## Now specify the exact models 
                 ## to evaluate:
                 tuneGrid = gbmGrid)
sales=predict(gbmFit1, test_home)
sub=cbind.data.frame(test_home$Id, sales)
colnames(sub) = c("Id", "SalePrice")
write_csv(sub, path = '~/Desktop/sub_housing_20dec_grid1.csv')


cat_var <- names(home_train)[which(sapply(home_train, is.character))]
cat_car <- c(cat_var, 'BedroomAbvGr', 'HalfBath', ' KitchenAbvGr','BsmtFullBath', 'BsmtHalfBath', 'MSSubClass')
numeric_var <- names(home_train)[which(sapply(home_train, is.numeric))]
colSums(sapply(home_train[,.SD, .SDcols = cat_var], is.na))
colSums(sapply(train[,.SD, .SDcols = numeric_var], is.na))
colSums(sapply(home_test, is.na)) ## check na in data
train_cat <- home_train[,.SD, .SDcols = cat_var]
train_cont <- home_train[,.SD,.SDcols = numeric_var]

make_factor = function(x) {
  c = as.factor(x)
  return(c)
}

for (i in names(home_test)){}


home_train[, ]


char.cols <- names(home_train)[sapply(home_train,is.character)]
for (name in char.cols){
  print(sprintf("Unique values for %s:", name))
  print(unique(home_train[[name]]))
  cat('\n')
}








plotHist <- function(data_in, i) {
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}

doPlots <- function(data_in, fun, ii, ncol=3) {
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}


plotDen <- function(data_in, i){
  data <- data.frame(x=data_in[[i]], SalePrice = data_in$SalePrice)
  p <- ggplot(data= data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_light() 
  return(p)
  
}

for(i in )
  
  
  
  