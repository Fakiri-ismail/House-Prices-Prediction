library(readr)
library(dplyr)
library(corrplot)
library(ggplot2)
library(caret)
library(mice)
library(VIM)
library(moments)
library(MASS)
library(tidyverse)
library(iterators)
library(parallel)
library(doMC)
library(data.table)
library(randomForest)
library(xgboost)
library(earth)
library(gridExtra)
library(scales)

# Lire les données des fichiers
train <- read_csv("Data/train.csv")
test  <- read_csv("Data/test.csv")
topredict<-read_csv("Data/sample_submission.csv")

#Creation des graphiques de la distribution des variables
plot1<- train %>% ggplot(.,aes(x = SalePrice)) +
  geom_area( stat = 'bin') + 
  ggtitle("Number of houses at each price point") +
  xlab("Sales price") +
  ylab("Number of houses") + 
  theme_minimal() + 
  scale_x_continuous(labels = dollar) 
plot2<- train %>% ggplot(.,aes(x = SalePrice)) +
  stat_ecdf(geom = 'step') +
  ggtitle("Proportion of dataset at each price point") +
  xlab("Sales price") +
  ylab("Proportion ") + 
  theme_minimal() + 
  scale_x_continuous(labels = dollar)
grid.arrange(plot1, plot2, nrow = 2)

#   Remove temporary variables used above
rm(list = c('plot1','plot2'))

#Creer une matrice de correlation seulement avec les 40 premieres colonnes
test2<-train[,2:40]
test2$SalePrice<-train$SalePrice

# Explore correlations with SalePrice
correlations <- test2[, sapply(test2, is.numeric)] %>%
  na.omit %>%
  cor
correlations %>%
  corrplot.mixed(
    lower = "circle",
    upper = "circle",
    tl.pos = "lt",
    diag = "n",
    order = "hclust",
    hclust.method = "complete"
  )
  
#Extraire les variables avec un taux de correlation eleve avec SalePrice
highcorr <- c(names(correlations[,'SalePrice'])[
  which(correlations[,'SalePrice'] > 0.5)],
  names(correlations[,'SalePrice'])[which(correlations[,'SalePrice'] < -0.2)])

highcorr <- highcorr[highcorr != 'SalePrice']

#Visualiser les variables qui sont plus correle avec Sale Price
correlations2 <- test2[, sapply(test2, is.numeric)] %>%
  dplyr::select(OverallQual,YearBuilt,YearRemodAdd,TotalBsmtSF,SalePrice)%>% #We added dplyr:: to tell R we are using the select function from that particular library
  na.omit %>%
  cor
correlations2 %>%
  corrplot.mixed(
    lower = "circle",
    upper = "circle",
    tl.pos = "lt",
    diag = "n",
    order = "hclust",
    hclust.method = "complete"
  )
  
#Pour fusionner les données du test et du train Pour travailler sur un dataset dans la preparation
all <- rbind(train %>% mutate(train = 1) %>% dplyr::select(-SalePrice), test %>% mutate(train = 0))

#Eliminer les colonnes avec plus de 50% NULL
colSums(is.na(all))
all=subset(all,select=-c(Alley,FireplaceQu,PoolQC,Fence,MiscFeature))

#Transformer les données categorique
all <- all %>%
  mutate(
    GarageFinish = as.factor(ifelse(is.na(GarageFinish), 'NA', GarageFinish)),
    GarageQual = as.factor(ifelse(is.na(GarageQual), 'NA', GarageQual)),
    GarageCond = as.factor(ifelse(is.na(GarageCond), 'NA', GarageCond)),
    GarageType = as.factor(ifelse(is.na(GarageType), 'NA', GarageType)),
    BsmtCond = as.factor(ifelse(is.na(BsmtCond), 'NA', BsmtCond)),
    BsmtExposure = as.factor(ifelse(is.na(BsmtExposure), 'NA', BsmtExposure)),
    BsmtQual = as.factor(ifelse(is.na(BsmtQual), 'NA', BsmtQual)),
    BsmtFinType2 = as.factor(ifelse(is.na(BsmtFinType2), 'NA', BsmtFinType2)),
    BsmtFinType1 = as.factor(ifelse(is.na(BsmtFinType1), 'NA', BsmtFinType1)),
    Electrical = as.factor(ifelse(is.na(Electrical), 'SBrkr', Electrical)),
    MSZoning = as.factor(ifelse(is.na(MSZoning), 'RL', MSZoning)),
    KitchenQual = as.factor(ifelse(is.na(KitchenQual), 'TA', KitchenQual)),
    Functional = as.factor(ifelse(is.na(Functional), 'Typ', Functional)),
    SaleType = as.factor(ifelse(is.na(SaleType), 'WD', SaleType)),
    Exterior1st = as.factor(ifelse(is.na(Exterior1st), 'VinylSd', Exterior1st)),
    Exterior2nd = as.factor(ifelse(is.na(Exterior2nd), 'VinylSd', Exterior2nd)),
    MasVnrType = as.factor(ifelse(is.na(MasVnrType), 'None', MasVnrType))
  ) %>%
  dplyr::select(
    -Utilities
  )
  
# Not numeric variables
all <- all %>% mutate(#mutate() adds new variables and preserves existing ones
  MSSubClass = as.factor(MSSubClass)
)

#replace missing data on a  dataset
#IN THIS SECTION we will replace categorical data NA with the frequently used in that particular column
all$MasVnrType<-replace_na(all$MasVnrType,tail(names(sort(table(all$MasVnrType))), 1))#It will be replaced with the frequent value of the column
all$BsmtQual<-replace_na(all$BsmtQual,tail(names(sort(table(all$BsmtQual))), 1))#It will be replaced with the frequent value of the column
all$BsmtCond<-replace_na(all$BsmtCond,tail(names(sort(table(all$BsmtCond))), 1))#It will be replaced with the frequent value of the column
all$BsmtExposure<-replace_na(all$BsmtExposure,tail(names(sort(table(all$BsmtExposure))), 1))#It will be replaced with the frequent value of the column
all$BsmtFinType1<-replace_na(all$BsmtFinType1,tail(names(sort(table(all$BsmtFinType1))), 1))#It will be replaced with the frequent value of the column
all$BsmtFinType2<-replace_na(all$BsmtFinType2,tail(names(sort(table(all$BsmtFinType2))), 1))#It will be replaced with the frequent value of the column
all$GarageType<-replace_na(all$GarageType,tail(names(sort(table(all$GarageType))), 1))#It will be replaced with the frequent value of the column
all$GarageYrBlt<-replace_na(all$GarageYrBlt,tail(names(sort(table(all$GarageYrBlt))), 1))#It will be replaced with the frequent value of the column
all$GarageFinish<-replace_na(all$GarageFinish,tail(names(sort(table(all$GarageFinish))), 1))#It will be replaced with the frequent value of the column
all$GarageQual<-replace_na(all$GarageQual,tail(names(sort(table(all$GarageQual))), 1))#It will be replaced with the frequent value of the column
all$GarageCond<-replace_na(all$GarageCond,tail(names(sort(table(all$GarageCond))), 1))#It will be replaced with the frequent value of the column
all$GarageCond<-replace_na(all$GarageCond,tail(names(sort(table(all$GarageCond))), 1))#It will be replaced with the frequent value of the column

#Remplacer les NA dans les valeurs numeriques avec la moyenne
all$LotFrontage = ifelse(is.na(all$LotFrontage),
                         ave(
                           all$LotFrontage,
                           FUN = function(x)
                             mean(x, na.rm = TRUE)
                         ),
                         all$LotFrontage)
all$LotFrontage <- as.integer(all$LotFrontage)
all$MasVnrArea = ifelse(is.na(all$MasVnrArea),
                         ave(
                           all$MasVnrArea,
                           FUN = function(x)
                             mean(x, na.rm = TRUE)
                         ),
                         all$MasVnrArea)
all$MasVnrArea <- as.integer(all$MasVnrArea)

all$TotalBsmtSF = ifelse(is.na(all$TotalBsmtSF),
                        ave(
                          all$TotalBsmtSF,
                          FUN = function(x)
                            mean(x, na.rm = TRUE)
                        ),
                        all$TotalBsmtSF)
all$TotalBsmtSF <- as.integer(all$TotalBsmtSF)
all$BsmtFinSF1 = ifelse(is.na(all$BsmtFinSF1),
                         ave(
                           all$BsmtFinSF1,
                           FUN = function(x)
                             mean(x, na.rm = TRUE)
                         ),
                         all$BsmtFinSF1)
all$BsmtFinSF1 <- as.integer(all$BsmtFinSF1)
all$BsmtFullBath <- replace_na(all$BsmtFullBath,1)
all$BsmtFinSF2 <- replace_na(all$BsmtFinSF2,0)
all$BsmtHalfBath <- replace_na(all$BsmtHalfBath,0)
all$GarageCars <- replace_na(all$GarageCars,3)
all$BsmtUnfSF = ifelse(is.na(all$BsmtUnfSF),
                        ave(
                          all$BsmtUnfSF,
                          FUN = function(x)
                            mean(x, na.rm = TRUE)
                        ),
                        all$BsmtUnfSF)
all$BsmtUnfSF <- as.integer(all$BsmtUnfSF)
all$GarageArea = ifelse(is.na(all$GarageArea),
                       ave(
                         all$GarageArea,
                         FUN = function(x)
                           mean(x, na.rm = TRUE)
                       ),
                       all$GarageArea)
all$GarageArea <- as.integer(all$GarageArea)

####Take off later
# Correct skewness of numeric variables

feature_classes <- sapply(names(all), function(x) {
  class(all[[x]])
})

numeric_feats <- names(feature_classes[feature_classes == "integer"])

skewed_feats <- sapply(numeric_feats, function(x) {
  skewness(all[[x]], na.rm = TRUE)
})

## Keep only features that exceed a threshold (0.75) for skewness
skewed_feats <- skewed_feats[abs(skewed_feats) > 0.75]

## Transform skewed features with boxcox transformation
for (x in names(skewed_feats)) {
  bc = BoxCoxTrans(all[[x]], lambda = 0.15)
  all[[x]] = predict(bc, all[[x]])
}  


#TAKE OFF LATER
#Dummy variables
dmy <- dummyVars(" ~ .", data = all)
all <- data.frame(predict(dmy, newdata = all))

# Create new features
all <- all %>%
  mutate(
    TotalSF = TotalBsmtSF + X.1stFlrSF. + X.2ndFlrSF.
  )
  
#Separer les données
# Données d'entrainement
# train_trans <- all %>%
#   filter(train == 1) %>%
#   left_join(train %>% dplyr::select(Id, SalePrice)) %>%
#   mutate(
#     SalePrice = log1p(SalePrice)
#   ) %>%
#   dplyr::select(
#     -Id,
#     -train
#   )
train_trans <- all %>%
  filter(train == 1) %>%
  left_join(train %>% dplyr::select(Id, SalePrice)) %>%
  dplyr::select(
    -Id,
    -train
  )
  
# Données Test
test_trans <- all %>%
  filter(train == 0) %>%
  mutate(
    Id = as.factor(as.character(Id))
  ) %>%
  dplyr::select(
    -train
  )
  
#Exporter les données en un fichier excel
train_trans %>% write_csv(sprintf('output/train%s.csv', na = ''))
test_trans %>% write_csv(sprintf('output/test%s.csv', na = ''))

#Model Regression lineaire Multiple
train_regression <- fread('output/train.csv')
test <- fread('output/test.csv')
linear <- lm(SalePrice ~ ., data = train_regression)
summary(linear)

#Predire en utilisant une regression lineaire multiple
####predicted <- expm1(predict(linear, test))
predicted <- predict(linear, test)
output <- data.frame(test$Id, predicted,topredict$SalePrice)
colnames(output) <- cbind("Id", "PredictedSalePrice","RealPrice")

#Creation d'un fichier csv ou on ajoute l'ID la prediction et le prix reel
output %>% write_csv('submission/linear.csv')
glance(linear) %>%
  dplyr::select(adj.r.squared, sigma, AIC, BIC, p.value)
  
#Model Regression multiple apres elimination de variables
sum(is.na(train_regression))
step.model <- stepAIC(linear, direction = "backward", 
                      trace = FALSE)
predicted <- expm1(predict(step.model, test))
summary(step.model)
output <- data.frame(test$Id, predicted,topredict$SalePrice)
colnames(output) <- cbind("Id", "PredictedSalePrice","RealPrice")

#Creation d'un fichier csv ou on ajoute l'ID la prediction et le prix reel
output %>% write_csv('submission/linear2.csv')

#Creation du Model Random Forest
train_random_forest<-train_regression
test_random_forest <- test %>% as.data.frame()
rfFit <- randomForest(
  SalePrice ~. ,
  data = train_random_forest,
  method = "anova",
  ntree = 1500,
  mtry = 30,
  replace = F,
  importance = T
)
summary(rfFit)
predicted <- expm1(predict(rfFit, newdata = test))
output <- data.frame(test$Id, predicted,topredict$SalePrice)
colnames(output) <- cbind("Id", "SalePrice","RealPrice")
output %>% write_csv('submission/rf.csv')
plot(rfFit)#The error eliminating with more trees

#Creation du modele XGBOOST
train_Xgboost<-train_regression
test_Xgboost<-test
outcome <- c('SalePrice')
predictors <- names(train_Xgboost)[!names(train_Xgboost) %in% outcome]

mx.train <- as.matrix(train_Xgboost, rownames.force = NA)
mx.test <- as.matrix(test_Xgboost, rownames.force = NA)
train_dmatrix <- xgb.DMatrix(data = mx.train[, predictors], label = mx.train[, outcome] )

fit.xgb <- xgboost(
  data = train_dmatrix,
  nrounds = 20,
  objective = "reg:linear"
)

xgb.dump(fit.xgb)
predicted <- expm1(predict(fit.xgb, as.matrix(mx.test[, predictors])))
output <- data.frame(test$Id, predicted,topredict$SalePrice)
colnames(output) <- cbind("Id", "SalePrice","RealPrice")
output %>% write_csv('submission/xgb.csv')

#Fonction pour calculer R2
#1. MEAN ABSOLUTE PERCENTAGE ERROR (MAPE)
MAPE = function(y_actual,y_predict){
  mean(abs((y_actual-y_predict)/y_actual))*100
}

#2. R SQUARED error metric -- Coefficient of Determination
RSQUARE = function(y_actual,y_predict){
  cor(y_actual,y_predict)^2
}

#CREATING M.A.R.S model
mars1 <- earth(
  SalePrice ~ .,  
  data = train_regression
)
print(mars1)
plot(mars1, which = 1)
hyper_grid <- expand.grid(
  degree = 1:3, 
  nprune = seq(2, 100, length.out = 10) %>% floor()
)
head(hyper_grid)
set.seed(123)
tuned_mars <- train(
  x = subset(train_regression, select = SalePrice),
  y = train_regression$SalePrice,
  method = "earth",
  metric = "RMSE",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = hyper_grid
)
tuned_mars$bestTune
ggplot(tuned_mars)

#########################TEST#########################
#M.A.R.S Model
mars1 <- earth(
  SalePrice ~ .,  
  data = train_regression
)
print(mars1)
plot(mars1, which = 1)
hyper_grid <- expand.grid(
  degree = 1:3, 
  nprune = seq(2, 100, length.out = 10) %>% floor()
)
head(hyper_grid)
set.seed(123)
tuned_mars <- train(
  x = subset(train_regression, select = SalePrice),
  y = train_regression$SalePrice,
  method = "earth",
  metric = "RMSE",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = hyper_grid
)
tuned_mars$bestTune
ggplot(tuned_mars)
regression_prediction<- predict(mars1,train_regression[1000:1460])
RMSE.temp<-RMSE(regression_prediction,train_regression$SalePrice[1000:1460])
model_results <- bind_rows(model_results,
                           data_frame(Model="M.A.R.S TUNED",
                                      RMSE = RMSE.temp ))
									  
#Multiple Linear Regression
test <- fread('output/test.csv')
cv_model1 <- train(
  SalePrice ~ ., 
  data = train_regression, 
  method = "lm",
  metric = "RMSE",
  trControl = trainControl(method = "cv", number = 10),
  preProcess = c("zv", "center", "scale")
)
regression_prediction<- predict(cv_model1,test)
RMSE.temp<-RMSE(regression_prediction,topredict$SalePrice)
model_results <- bind_rows(model_results,
                           data_frame(Model="Multiple Linear Regression",
                                      RMSE = RMSE.temp ))
plot(cv_model1)

# principal component regression
set.seed(123)
cv_model2 <- train(
  SalePrice ~ ., 
  data = train_regression, 
  method = "pcr",
  trControl = trainControl(method = "cv", number = 10),
  metric = "RMSE",
  preProcess = c("zv", "center", "scale"),
  tuneLength = 20
)
regression_prediction<- predict(cv_model2,test)
RMSE.temp<-RMSE(regression_prediction,topredict$SalePrice)
model_results <- bind_rows(model_results,
                           data_frame(Model="ACP",
                                      RMSE = RMSE.temp ))
summary(cv_model2)

# partial least squares regression
set.seed(123)
cv_model3 <- train(
  SalePrice ~ ., 
  data = train_regression, 
  method = "pls",
  trControl = trainControl(method = "cv", number = 10),
  metric = "RMSE",
  preProcess = c("zv", "center", "scale"),
  tuneLength = 20
)
regression_prediction<- predict(cv_model3,test)
RMSE.temp<-RMSE(regression_prediction,topredict$SalePrice)
model_results <- bind_rows(model_results,
                           data_frame(Model="partial least squares",
                                      RMSE = RMSE.temp ))
summary(cv_model3)

# regularized regression
set.seed(123)
cv_model4 <- train(
  SalePrice ~ ., 
  data = train_regression,
  method = "glmnet",
  trControl = trainControl(method = "cv", number = 10),
  metric = "RMSE",
  preProcess = c("zv", "center", "scale"),
  tuneLength = 10
)
regression_prediction<- predict(cv_model4,train_regression[1000:1460])
RMSE.temp<-RMSE(regression_prediction,train_regression$SalePrice[1000:1460])
model_results <- data.frame( Model = 'regularized regression', 
                             RMSE = RMSE.temp)
summary(cv_model4)
summary(resamples(list(
  Multiple_regression = cv_model1, 
  PCR = cv_model2, 
  PLS = cv_model3,
  Elastic_net = cv_model4
)))

# extract out of sample performance measures
summary(resamples(list(
  Multiple_regression = cv_model1, 
  PCR = cv_model2, 
  PLS = cv_model3,
  Elastic_net = cv_model4
)))$statistics$RMSE %>%
  kableExtra::kable() %>%
  kableExtra::kable_styling(bootstrap_options = c("striped", "hover"))
  
######################################################
#Calculer R2 pour ensemble de modele
########Regression multiple###########
output_regression_multiple<- read.csv('submission/linear.csv')
prid<-data.frame(pred=output_regression_multiple$PredictedSalePrice,real=output_regression_multiple$RealPrice)
MAPE(output_regression_multiple$RealPrice,output_regression_multiple$PredictedSalePrice)
RSQUARE(output_regression_multiple$RealPrice,output_regression_multiple$PredictedSalePrice)
data.frame(
  R2 = R2(output_regression_multiple$PredictedSalePrice, output_regression_multiple$RealPrice),
  RMSE = RMSE(output_regression_multiple$PredictedSalePrice, output_regression_multiple$RealPrice),
  MAE = MAE(output_regression_multiple$PredictedSalePrice, output_regression_multiple$RealPrice)
)
glance(linear)
summary(linear)

#######Random Forest###########
output_random_forest<- read.csv('submission/rf.csv')
MAPE(output_random_forest$RealPrice,output_random_forest$SalePrice)
RSQUARE(output_random_forest$RealPrice,output_random_forest$SalePrice)

######XGBOOST###########
output_XGBOOST<- read.csv('submission/xgb.csv')
MAPE(output_XGBOOST$RealPrice,output_XGBOOST$SalePrice)
RSQUARE(output_XGBOOST$RealPrice,output_XGBOOST$SalePrice)
RMSE <- function(true_ratings, predicted_ratings){
  
  sqrt(mean((true_ratings - predicted_ratings)^2))
  
}