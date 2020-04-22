library(readxl)
library(forecast)
library(leaps)
library(MASS)
library(tidyverse)
library(forcats)
library(recipes)
library(lubridate)
library(corrplot)
library(scales)
library(ranger)
library(randomForest)
library(purrr)
library(knitr)
library(randomForestSRC)
library(caret)
library(rsample)
setwd("C:/Users/n077511/Box Sync/WEMBA DOCS/Courses/Forecasting and Predictive Analytics/Project")
iowatrain = read.csv("train.csv")
iowatest = read.csv("test.csv")
iowafullset = read.csv("fullset.csv")
## making sure these columns are numeric and not factors
iowafullset$LotFrontage = as.numeric(as.character(iowafullset$LotFrontage))
iowafullset$MasVnrArea = as.numeric(as.character(iowafullset$MasVnrArea))
iowafullset$GarageYrBlt = as.numeric(as.character(iowafullset$GarageYrBlt))
##Identifying which factor columns have NAs to be recoded
sapply(iowafullset, function(x) sum(is.na(x)))
## levels in Factor columns
iowafullset$Alley = forcats::fct_explicit_na(iowafullset$Alley, "No Access")
iowafullset$MasVnrType = forcats::fct_explicit_na(iowafullset$MasVnrType,"")
iowafullset$BsmtQual = forcats::fct_explicit_na(iowafullset$BsmtQual,"No Basement")
iowafullset$Bsmtcondl = forcats::fct_explicit_na(iowafullset$BsmtCond,"No Basement")
iowafullset$BsmtExposure = forcats::fct_explicit_na(iowafullset$BsmtExposure,"No Basement")
iowafullset$BsmtFinType1 = forcats::fct_explicit_na(iowafullset$BsmtFinType1,"No Basement")
iowafullset$BsmtFinType2 = forcats::fct_explicit_na(iowafullset$BsmtFinType2,"No Basement")
iowafullset$Electrical = forcats::fct_explicit_na(iowafullset$Electrical,"")
iowafullset$FireplaceQu = forcats::fct_explicit_na(iowafullset$FireplaceQu,"No Fireplace")
iowafullset$GarageType = forcats::fct_explicit_na(iowafullset$GarageType,"No Garage")
iowafullset$GarageFinish = forcats::fct_explicit_na(iowafullset$GarageFinish,"No Garage")
iowafullset$GarageQual = forcats::fct_explicit_na(iowafullset$GarageQual,"No Garage")
iowafullset$GarageCond = forcats::fct_explicit_na(iowafullset$GarageCond,"No Garage")
iowafullset$PoolQC = forcats::fct_explicit_na(iowafullset$PoolQC,"No Pool")
iowafullset$Fence = forcats::fct_explicit_na(iowafullset$Fence,"No Fence")
iowafullset$MiscFeature = forcats::fct_explicit_na(iowafullset$MiscFeature,"None")
## Removing NA in the numeric columns
iowafullset = tidyr::replace_na(iowafullset, list(LotFrontage = "", MasVnrArea = "", GarageYrBlt = ""))
## Linear Regression Model. 

iowafullset %>% 
  ggplot(aes(x = SalePrice, fill = Neighborhood)) +
  geom_histogram() +
  facet_wrap(~Neighborhood)

iowafullset %>% 
  ggplot(aes(x = BldgType, y = SalePrice)) +
  geom_boxplot()

reg = lm(SalePrice ~. -Id -Utilities -Condition2 -LotFrontage, data = iowafullset)
summary(reg)


fullsetv2 <-
  iowafullset %>% 
  mutate(porchtype = case_when(WoodDeckSF > 0 ~ "wooddeck",
                               OpenPorchSF > 0 ~ "openporch",
                               EnclosedPorch > 0 ~ "enclosed",
                               "3SsnPorch" > 0 ~ "threeseason",
                               ScreenPorch > 0 ~ "screen"),
         finishedbsmtsqft = TotalBsmtSF - BsmtUnfSF,
         totalfullbaths = FullBath + BsmtFullBath) %>% 
  select(Id,
         SalePrice,
         porchtype,
         PavedDrive,
         PoolArea,
         KitchenQual,
         MSZoning,
         LotArea,
         RoofStyle,
         TotalBsmtSF,
         finishedbsmtsqft,
         BsmtFinType1,
         BedroomAbvGr,
         totalfullbaths,
         Fireplaces,
         FireplaceQu,
         SaleCondition,
         Neighborhood,
         MSSubClass,
         Utilities,
         OverallQual,
         OverallCond,
         ExterQual,
         ExterCond,
         HeatingQC,
         CentralAir,
  ) 




recipe2 <- recipe(SalePrice ~ ., data = fullsetv2) 

recipe2steps <-
  recipe2 %>% 
  step_dummy(all_nominal()) 

preppedrecipe2 <- prep(recipe2steps, training = fullsetv2, strings_as_factors = TRUE)

fullsetbaked <-
  bake(preppedrecipe2,fullsetv2)

#Split into train and test set 70/30 train/test to be used in linear regression model

set.seed(343)

splitdata <- 
  fullsetv2 %>% 
  initial_split(prop = 0.7)

trainv2 <-
  training(splitdata)
testv2 <-
  testing(splitdata)

reg = lm(SalePrice ~., data = trainv2)
summary(reg)

pred = predict(reg, new = trainv2)
accuracy(pred, trainv2$SalePrice)

pred = predict(reg, new = testv2)
accuracy(pred, testv2$SalePrice)
