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
plot(total$SalePrice, total$GrLivArea)
total$OverallCond = as.factor(total$OverallCond)
total$OverallQual= as.factor(total$OverallQual)
#total$YearBuilt = as.character(as.numeric(total$YearBuilt))
#total$YearRemodAdd = as.character(as.numeric(total$YearRemodAdd))

total$year_feature = ifelse(total$YearBuilt <= 1900, 1,
                            ifelse(total$YearBuilt >= 1901 & total$YearBuilt <= 1930, 2,
                                   ifelse(total$YearBuilt >= 1931 & total$YearBuilt <= 1960, 3,
                                          ifelse(total$YearBuilt >= 1961 & total$YearBuilt <= 1990, 4,
                                                 5))))


total$year_feature2 = ifelse(total$YearRemodAdd <= 1950, 1,
                             ifelse(total$YearRemodAdd >= 1951 & total$YearRemodAdd <= 2000, 2,
                                    3))


total$year_feature3_diff = total$YrSold - total$YearBuilt
total$year_feature4_diff_remod = total$YrSold - total$YearRemodAdd
total$year_feature = as.factor(total$year_feature)
total$year_feature2 = as.factor(total$year_feature2)




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
total$MSZoning = as.factor(total$MSZoning)
total$LotShape = ifelse(total$LotShape == "Reg", 1, 2)
total$LotShape = as.factor(total$LotShape)
total$LandContour = ifelse(total$LandContour == "Lvl", 1, 2)
total$LandContour = as.factor(total$LandContour)
total$LotConfig = ifelse(total$LotConfig == "Inside", 1, 2)
total$LotConfig = as.factor(total$LotConfig)
total$LandSlope = ifelse(total$LandSlope == "Gtl", 1, 2)
total$LandSlope = as.factor(total$LandSlope)
total$Condition1 = ifelse(total$Condition1 == "Norm", 1, 2)
total$Condition1 = as.factor(total$Condition1)
total$Condition2 = as.factor(total$Condition2)
total$Condition2 = ifelse(total$Condition2 == "Norm", 1, 2)
total$HouseStyle = ifelse(total$HouseStyle == "1Story", 1,
                          ifelse(total$HouseStyle == "2Story", 2,
                                 3))
total$HouseStyle = as.factor(total$HouseStyle)
total$RoofMatl = ifelse(total$RoofMatl == "CompShg", 1, 2)
total$RoofMatl = as.factor(total$RoofMatl)
total$RoofStyle = ifelse(total$RoofStyle == "Gable", 1,
                         ifelse(total$RoofStyle == "Hip", 2,
                                3))
total$RoofStyle = as.factor(total$RoofStyle)

total$SaleCondition = ifelse(total$SaleCondition=="Normal", 1, 2)
total$SaleType = ifelse(total$SaleType == "WD", 1, 
                        ifelse(total$SaleType == "New", 2, 
                               3))

total$GarageQual = ifelse(total$GarageQual == "TA", 1, 2)
#assign factors to gargaedate

total$year_garage2 = ifelse(total$GarageYrBlt <= 1950, 1,
                            ifelse(total$GarageYrBlt >= 1951 & total$GarageYrBlt <= 2000, 2,
                                   3))

##asgin remod year
total$remodel_feature = ifelse(total$YearRemodAdd == total$YrSold, 1, 2)
total$remodel_feature = as.factor(total$remodel_feature)
#convert other categoricals into factors
total$Exterior1st = as.factor(total$Exterior1st)
total$Exterior2nd = as.factor(total$Exterior2nd)
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
total$remodel_feature = as.factor(total$remodel_feature)
total$year_feature3_diff = as.factor(total$year_feature3_diff)
total$year_feature4_diff_remod = as.factor(total$year_feature4_diff_remod)
total$YrSold = as.factor(total$YrSold)
total$BsmtHalfBath = as.factor(total$BsmtHalfBath)
total$BsmtFullBath = as.factor(total$BsmtFullBath)
total$FullBath = as.factor(total$FullBath)
total$HalfBath = as.factor(total$HalfBath)
total$BedroomAbvGr = as.factor(total$BedroomAbvGr)
total$KitchenAbvGr = as.factor(total$KitchenAbvGr)
total$TotRmsAbvGrd = as.factor(total$TotRmsAbvGrd)
total$MoSold = as.factor(total$MoSold)
total$SaleCondition = as.factor(total$SaleCondition)
total$SaleType = as.factor(total$SaleType)
colnames(total)[43:44]=c("floor1_SF", "floor2_SF")
colnames(total)[68] =c("Porch_3ssn")
##split into test and train
require(dplyr)
train_home = total[1:1460,]
train_home = filter(train_home, train_home$GrLivArea <= 4000) #remove outliers
test_home = total[1461:nrow(total),]

mod1=glm(SalePrice~SaleCondition+BldgType+GrLivArea+remodel_feature+LotArea+BsmtFullBath+GarageCars+GarageType+Neighborhood+OverallQual+BsmtExposure+BsmtFinSF1+BsmtFinSF2+KitchenQual+ScreenPorch+year_feature+year_garage2, data = train_home)
summary(mod1)
sales=predict(mod1, test_home)
sub=cbind.data.frame(test_home$Id, sales)
colnames(sub) = c("Id", "SalePrice")
write_csv(sub, path = '~/Desktop/sub_housing_21dec_2.csv')

mod2=glm(SalePrice~., data = train_home[-c(1, 2, 73, 6, 9, 58, 19, 20)])
summary(mod2)
sales=predict(mod2, test_home)
sub=cbind.data.frame(test_home$Id, sales)
colnames(sub) = c("Id", "SalePrice")
write_csv(sub, path = '~/Desktop/sub_housing_19dec_423pm.csv')


require(randomForest)
rf_model <- randomForest(SalePrice~., data = train_home[-c(1, 2, 73, 6, 9, 58, 19, 20)])
predict(rf_model, test_home)
importance=importance(rf_model)

varImportance <- data.frame(Variables = row.names(importance), 
Importance = round(importance[ ,1]))

# Create a rank variable based on importance
rankImportance <- varImportance %>% mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), y = Importance, fill = Importance)) +geom_bar(stat='identity') + geom_text(aes(x = Variables, y = 0.5, label = Rank),hjust=0, vjust=0.55, size = 4, colour = 'red') +labs(x = 'Variables') +coord_flip()

sales=predict(rf_model, test_home)
sub=cbind.data.frame(test_home$Id, sales)
colnames(sub) = c("Id", "SalePrice")
write_csv(sub, path = '~/Desktop/sub_housing_23dec_rfmodel1.csv')



library(caret)
set.seed(998)
train_home2=train_home[-c(1, 73, 6, 9, 58, 19, 20)]
test_home2 =train_home[-c(1, 73, 6, 9, 58, 19, 20)]
test_mm2 = model.matrix(~., test_home2)
mm1= model.matrix(~., train_home2)
price = train_home$SalePrice/1000
require(glmnet)
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
gbmFit1 <- train(SalePrice ~., data = training, 
                 method = "gbm", 
                 trControl = fitControl,
                 train.fraction = 0.5,
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
write_csv(sub, path = '~/Desktop/sub_housing_23dec_grid1.csv')


###simple XGB boost

mmsparse = sparse.model.matrix(train_home$SalePrice~. -1,data=train_home[-c(1, 73, 6, 9, 58, 19, 20)])
data1=Matrix(mmsparse, sparse = T)
train1= list(train_home$SalePrice, mmsparse)

names(train1) = c("label", "data")
bstSparse=xgboost(data = train1$data, label = train1$label, max.depth = 15, alpha =0.01, nthread = 8, nround = 150, objective = 'reg:linear', num_parallel_tree = 100)



mmsparse2 = sparse.model.matrix(~.-1,data=test_home[-c(1, 73, 76, 6, 9, 58, 19, 20)])
sales = predict(bstSparse, mmsparse2)
sub=cbind.data.frame(test_home$Id, sales)
colnames(sub) = c("Id", "SalePrice")
write_csv(sub, path  = '~/Desktop/sub_housing_23dec_XGBboost3.csv')

require(caret)
require(xgboost)
### train an XGB boost model - split training data into test and training by caret package
inTraining <- createDataPartition(train_home$SalePrice, p = .50, list = FALSE)
training <- train_home[ inTraining,]
testing  <-train_home[-inTraining,]
train = sparse.model.matrix(training$SalePrice ~. -1, data=training[-c(1, 2, 73, 6, 9, 58, 19, 20)])
test = sparse.model.matrix(testing$SalePrice~. -1, data= testing[-c(1,2, 73, 6, 9, 58, 19, 20)])
trainlabel = training$SalePrice
testlabel = testing$SalePrice

trainlist = list(trainlabel, train)
testlist = list(testlabel, test)

names(trainlist)= c("label", "data")
names(testlist)= c("label", "data")

dtrain <- xgb.DMatrix(data = trainlist$data, label=trainlist$label)
dtest <- xgb.DMatrix(data = testlist$data, label=testlist$label)

watchlist <- list(train=dtrain, test=dtest)

bst <- xgb.train(data=dtrain, max_depth=15,  alpha = 0.01, nthread = 8, objective = 'reg:linear',  nrounds=200, watchlist=watchlist, verbose = T, num_parallel_tree = 100)


model <- xgb.dump(bst, with.stats = T)
names=dimnames(train1)[[2]]
importance_matrix <- xgb.importance(names, model = bst)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)
test_sparse = sparse.model.matrix(~.-1, data=test_home[-c(1, 73, 6, 9, 58, 19, 20, 76)])
sales = predict(bst, test_sparse)
sub=cbind.data.frame(test_home$Id, sales)
colnames(sub) = c("Id", "SalePrice")
write_csv(sub, path = '~/Desktop/sub_housing_22dec_XGBboost1.csv')
##predict
test_home = as.data.frame(test_home)
mmsparse2 = model.matrix(~.,test_home[-c(1, 73, 76, 6, 9, 58, 19, 20)])
data2 = Matrix(mmsparse2, sparse = T)
sales = predict(bst, data2)
sub=cbind.data.frame(test_home$Id, sales)
colnames(sub) = c("Id", "SalePrice")
write_csv(sub, path = '~/Desktop/sub_housing_23dec_XGBboost1.csv')

xgb_params = list(
  verbose =T,
  colsample_bytree = 0.5,
  subsample = 0.8,
  eta = 0.02, 
  objective = 'reg:linear',
  max_depth = 12,
  alpha = 1,
  gamma = 2,
  min_child_weight = 1,
  base_score = 7.76
)

xg_eval_mae <- function (yhat, dtrain) {
  y = getinfo(dtrain, "label")
  err= mae(exp(y),exp(yhat) )
  return (list(metric = "error", value = err))
}

best_n_rounds=15000 # try more rounds

#train data
gb_dt=xgb.train(xgb_params,dtrain,nrounds = as.integer(best_n_rounds))











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
  
  
  
  