##########################################################
# Loading required packages and database creation
##########################################################

# Loading or install the required packages

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) 
  install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(scales)) 
  install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) 
  install.packages("gridExtra", repos = "http://cran.us.r-project.org")
if(!require(ggcorrplot))
  install.packages("ggcorrplot", repos = "http://cran.us.r-project.org")
if(!require(ggplot2))
  install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot))
  install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(rpart))
  install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(C50))
  install.packages("C50", repos = "http://cran.us.r-project.org")
if(!require(randomForest))
  install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(caret))
  install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ROCR))
  install.packages("ROCR", repos = "http://cran.us.r-project.org")
if(!require(Rborist))
  install.packages("Rborist", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(grid)
library(gridExtra)
library(ggplot2)
library(ggcorrplot)
library(rpart)
library(rpart.plot)
library(C50)
library(randomForest)
library(caret)
library(ROCR)
library(Rborist)

# Database creation 

ci <- tempfile()
download.file("https://github.com/paugmataix/Choose-your-own-edx-cars/raw/main/Car_Insurance_Claim.csv",ci)

cars_insurance <- str_split_fixed(readLines(ci), "\\,",19)
colnames(cars_insurance) <- cars_insurance[1,]
cars_insurance <- cars_insurance[-1,]

cars_insurance <- as.data.frame(cars_insurance)

##########################################################
# Exploring cars_insurance dataset
##########################################################

# I want to know the names of columns of cars_insurance dataset
colnames(cars_insurance)

# How many rows are there in the movielens dataset?
nrow(cars_insurance)

# I want to know more information about the variables and what 
# are their possible values to define if they are categorical 
# or ordinal for further analysis
summary(cars_insurance)

# Each record have a different ID, so it doesn't provide us with useful information.

# As the SPEEDING_VIOLATIONS, CREDIT_SCORE, ANNUAL_MILEAGE and PAST_ACCIDENTS
# variables have more than six different values too, they are formatted 
# as a table to see them all values and see how to proceed with it.
table(SPEEDING_VIOLATIONS = cars_insurance$SPEEDING_VIOLATIONS)
table(PAST_ACCIDENTS = cars_insurance$PAST_ACCIDENTS)
table(ANNUAL_MILEAGE = cars_insurance$ANNUAL_MILEAGE)

# SPEEDING_VIOLATIONS, CREDIT_SCORE, ANNUAL_MILEAGE and PAST_ACCIDENTS are 
# passed to numerical to be able to analyze them
cars_insurance <- cars_insurance %>% mutate(SPEEDING_VIOLATIONS = 
 as.numeric(as.character(SPEEDING_VIOLATIONS)), PAST_ACCIDENTS = 
 as.numeric(as.character(PAST_ACCIDENTS)), ANNUAL_MILEAGE = 
   as.numeric(as.character(ANNUAL_MILEAGE)), CREDIT_SCORE = 
   as.numeric(as.character(CREDIT_SCORE)))

# Another important fact is to observe what proportion of claims in
# the sample have a value of 1 (the client has claimed his loan) and 
# what proportion have a value of 0 (the client has not claimed his loan).
cars_insurance %>% ggplot(aes(OUTCOME, fill=OUTCOME)) + geom_bar()

# It proceed to graphically observe the distribution according to the 
# other categorical variables

# Variable AGE
a <- cars_insurance %>% ggplot(aes(AGE, fill=OUTCOME)) + geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Variable GENDER
b <- cars_insurance %>% ggplot(aes(GENDER, fill=OUTCOME)) + geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Variable RACE
c <- cars_insurance %>% ggplot(aes(RACE, fill=OUTCOME)) + geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Variable DRIVING_EXPERIENCE
d <- cars_insurance %>% ggplot(aes(DRIVING_EXPERIENCE, fill=OUTCOME)) + geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Variable EDUCATION
e <- cars_insurance %>% ggplot(aes(EDUCATION, fill=OUTCOME)) + geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Variable INCOME
f <- cars_insurance %>% ggplot(aes(INCOME, fill=OUTCOME)) + geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Variable VEHICLE_OWNERSHIP
g <- cars_insurance %>% ggplot(aes(VEHICLE_OWNERSHIP, fill=OUTCOME)) + geom_bar()

# Variable VEHICLE_YEAR
h <- cars_insurance %>% ggplot(aes(VEHICLE_YEAR, fill=OUTCOME)) + geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Variable MARRIED
i <- cars_insurance %>% ggplot(aes(MARRIED, fill=OUTCOME)) + geom_bar()

# Variable CHILDREN
j <- cars_insurance %>% ggplot(aes(CHILDREN, fill=OUTCOME)) + geom_bar()

# Variable POSTAL_CODE
k <-cars_insurance %>% ggplot(aes(POSTAL_CODE, fill=OUTCOME)) + geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Variable VEHICLE_TYPE
l <- cars_insurance %>% ggplot(aes(VEHICLE_TYPE, fill=OUTCOME)) + geom_bar()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Variable SPEEDING_VIOLATIONS
m <- cars_insurance %>% ggplot(aes(SPEEDING_VIOLATIONS, fill=OUTCOME)) + geom_bar()

# Variable DUIS
n <- cars_insurance %>% ggplot(aes(DUIS, fill=OUTCOME)) + geom_bar()

# Variable PAST_ACCIDENTS
o <- cars_insurance %>% ggplot(aes(PAST_ACCIDENTS, fill=OUTCOME)) + geom_bar()

# Variable ANNUAL_MILEAGE
p <- cars_insurance %>% ggplot(aes(ANNUAL_MILEAGE, fill=OUTCOME)) + geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

grid.arrange(a,b,c,d,e,f,g,h, ncol = 2, nrow=4)
grid.arrange(i,j,k,l,m,n,o,p, ncol = 2, nrow=4)

# It is observed how the credit score is distributed according to the 
# outcome value
cars_insurance %>%
  ggplot(aes(OUTCOME, CREDIT_SCORE)) + geom_boxplot()

# The variables are transformed to proceed later with the correlation matrix
c_i <- cars_insurance
c_i <- c_i[-1]
attach(c_i)

MARRIED = as.numeric(MARRIED,levels=c("0.0","1.0"), labels = 0,1)
c_i$MARRIED <- MARRIED

CHILDREN = as.numeric(CHILDREN,levels=c("0.0","1.0"), labels = 0,1)
c_i$CHILDREN <- CHILDREN

VEHICLE_OWNERSHIP   = as.numeric(VEHICLE_OWNERSHIP,levels=c("0.0","1.0"), labels = 0,1)
c_i$VEHICLE_OWNERSHIP <- VEHICLE_OWNERSHIP

OUTCOME             = as.numeric(OUTCOME,levels=c("0.0","1.0"), labels = 0,1)
c_i$OUTCOME         <- OUTCOME

AGE                 = as.numeric(factor(AGE,levels=c("16-25","26-39","40-64","65+")))
c_i$AGE             <- AGE

GENDER              = as.numeric(factor(GENDER,levels=c("male", "female")))
c_i$GENDER          <- GENDER

RACE                = as.numeric(factor(RACE, levels=c("majority","minority")))
c_i$RACE            <- RACE

DRIVING_EXPERIENCE  = as.numeric(factor(DRIVING_EXPERIENCE, levels=c("0-9y","10-19y","20-29y","30y+")))
c_i$DRIVING_EXPERIENCE <- DRIVING_EXPERIENCE

EDUCATION           = as.numeric(factor(EDUCATION, levels=c("high school","none","university")))
c_i$EDUCATION       <- EDUCATION

INCOME              = as.numeric(factor(INCOME, levels=c("middle class","poverty","upper class","working class")))
c_i$INCOME          <- INCOME

VEHICLE_YEAR        = as.numeric(factor(VEHICLE_YEAR, levels=c("after 2015","before 2015")))
c_i$VEHICLE_YEAR    <- VEHICLE_YEAR

POSTAL_CODE         = as.numeric(factor(POSTAL_CODE, levels=c("10238","21217","32765","92101")))
c_i$POSTAL_CODE     <- POSTAL_CODE

VEHICLE_TYPE        = as.numeric(factor(VEHICLE_TYPE, levels=c("sedan","sports car")))
c_i$VEHICLE_TYPE    <- VEHICLE_TYPE

c_i$ANNUAL_MILEAGE <- as.numeric(as.character(c_i$ANNUAL_MILEAGE))
c_i$SPEEDING_VIOLATIONS <- as.numeric(as.character(c_i$SPEEDING_VIOLATIONS))
c_i$DUIS <- as.numeric(as.character(c_i$DUIS))
c_i$PAST_ACCIDENTS  <- as.numeric(as.character(c_i$PAST_ACCIDENTS))
c_i$CREDIT_SCORE <- as.numeric(as.character(c_i$CREDIT_SCORE))

# Select numeric variables
df <- dplyr::select_if(c_i, is.numeric)

# Calulate the correlation matrix
r <- cor(df, use="pairwise.complete.obs")
r <- round(r,2)
ggcorrplot(r, 
           method = c("square", "circle"), 
           type = c("full","lower", "upper"),
           ggtheme = ggplot2::theme_minimal, title = "",
           show.legend = TRUE, 
           legend.title = "Corr", 
           show.diag = FALSE,
           colors = c("blue", "white", "red"), 
           outline.color = "gray",
           hc.order = TRUE, 
           hc.method = "complete", 
           lab = FALSE,
           lab_col = "black", 
           lab_size = 2, 
           p.mat = NULL, 
           sig.level = 0.05,
           insig = c("pch", "blank"), 
           pch = 4, pch.col = "black",
           pch.cex = 5, 
           tl.cex = 5, 
           tl.col = "black", 
           tl.srt = 45,
           digits = 2)

# The variables for the models are transformed
c_i$OUTCOME <- factor(c_i$OUTCOME)
c_i$AGE <- factor(c_i$AGE)
c_i$GENDER <- factor(c_i$GENDER)
c_i$RACE <- factor(c_i$RACE)
c_i$DRIVING_EXPERIENCE <- factor(c_i$DRIVING_EXPERIENCE)
c_i$EDUCATION <- factor(c_i$EDUCATION)
c_i$INCOME <- factor(c_i$INCOME)
c_i$VEHICLE_YEAR <- factor(c_i$VEHICLE_YEAR)
c_i$POSTAL_CODE <- factor(c_i$POSTAL_CODE)
c_i$VEHICLE_TYPE   <- factor(c_i$VEHICLE_TYPE)

# Create the training and test dataset
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = c_i$OUTCOME, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- c_i[-test_index,]
test_set <- c_i[test_index,]

train_set <- train_set[complete.cases(train_set), ]
test_set <- test_set[complete.cases(test_set), ]

# Obtaining the best accuracy for the classification tree
train_rpart <- train(OUTCOME ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                     data = train_set)
cp_tree <- train_rpart$results$cp[which.max(train_rpart$results$Accuracy)] 
cp_tree
plot(train_rpart)

# Classification tree creation
train_rpart <- rpart(OUTCOME~.,method="class",data=train_set)

# Classification tree visualization
rpart.plot(train_rpart,extra = 4) 

# Pruning the classification tree
pArbolRpart <- prune(train_rpart, cp=cp_tree)

# Confusion matrix of the classification tree
testPredRpart <- predict(train_rpart, newdata = test_set, type="class")
table(testPredRpart,test_set$OUTCOME)

# Efectivity of the classification tree
sum(testPredRpart==test_set$OUTCOME)/length(test_set$OUTCOME)*100

# Obtaining the best tune for the random forest
train_rf_2 <- train(OUTCOME ~ .,
                    method = "Rborist",
                    tuneGrid = data.frame(predFixed = c(2,3,4,5,7,10,15,20), minNode = 2),
                    data = train_set)
train_rf_2$bestTune
plot(train_rf_2)

# Random forest creation
mod <- randomForest(OUTCOME ~ .,
                    data=train_set,
                    nTree = 5000,
                    minNode = train_rf_2$bestTune$minNode,
                    predFixed = train_rf_2$bestTune$predFixed )

# Importance of variables
importance(mod)[order(importance(mod)),]
varImpPlot(mod)

# Confusion matrix of the random forest
confusionMatrix(predict(train_rf_2, test_set), test_set$OUTCOME)

