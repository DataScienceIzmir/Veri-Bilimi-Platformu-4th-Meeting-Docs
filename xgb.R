## LOAD THE LIBS ----

load.libraries <- c('tidyverse', 'forcats', 'stringr','DT', 'data.table', 'caTools', 'magrittr', 'janitor', 'scales', 'directlabels', 
                    'ggthemes', 'plotly', 'tidyr', 'dplyr', 'scales', 'grid', 'gridExtra', 'corrplot','VIM', 'knitr', 'vcd', 'caret', 'lubridate',
                    'ggrepel', 'reshape2', 'data.table', 'testthat', 'gridExtra', 'corrplot', 'GGally', 'ggplot2', 'e1071',
                    "rCharts", "TTR", "shinydashboard", "forecast", 'MLmetrics', 'randomForest', 'rpart', 'rpart.plot', 'car', 'e1071',
                    'ROCR', 'pROC', 'glmnet', 'xgboost', 'h2o', 'irlba', 'moments', 'readr')
sapply(load.libraries, require, character = TRUE)


get_dup <- function(x) lapply(x, c) %>% duplicated %>% which 

# DATA PREPROCESSING-------------

#read data

train <- read_csv("D:/MeetUp_AutoData.csv") 
#test <- read_csv("RAcredit_test.csv")
str(train)
numerical <- c("price", "wheel-base", "length", "width","height", "normalized-losses", "curb-weight","engine-size", "bore", "stroke", "compression-ratio", "horsepower",
               "peak-rpm", "city-mpg", "highway-mpg")

#recode as numeric
train[, which(names(train) %in% numerical)] <- sapply(train[, which(names(train) %in% numerical)], as.numeric)
train$symboling <- as.character(train$symboling)

str(train)
# Unique values per column ----
#len <- t(data.frame(lapply(train[, -which(names(train) %in% numerical)], function(x) length(unique(x)))))
#View(len)
library(skimr)
skim_to_list(train)


#Check for Missing values
missing_values <- train %>% summarize_all(funs(sum(is.na(.))/n()))
#missing_values <- test %>% summarize_all(funs(sum(is.na(.))/n()))

missing_values <- gather(missing_values, key="feature", value="missing_pct")
missing_values %>% 
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
  geom_bar(stat="identity",fill="red")+
  coord_flip()+theme_bw() + labs(x='features', y='% missing', title='Percent missing data by feature')


#nice one to analyze the data
#datatable(checkAllCols(train), style="bootstrap", class="table-condensed", options = list(dom = 'tp',scrollX = TRUE))

cols <- sapply(train, class)

numeric_cols <- rownames(data.frame(cols[which(cols != "character")]))
char_cols <- rownames(data.frame(cols[which(cols == "character")]))

#impute missing variables with regression
library(mice)
#miceMod <- mice(train[, !names(train) %in% "price"], method="rf", m = 5) #random forest
miceMod2 <- mice(train[, !names(train) %in% "price"], diagnostics = T) #regression (pmm)

# miceOutput <- mice::complete(miceMod)
miceOutput2 <- mice::complete(miceMod2)

#check missings again
missdata <- data.frame(cbind(train$`normalized-losses`, miceOutput2$`normalized-losses`))
colnames(missdata) <- c("Originals", "LR_Model")

plot_ly(missdata, x =~1:nrow(missdata), y = ~Originals, type = "scatter", mode = 'lines', name = 'Originals') %>%
  #add_trace(y = ~RF_Model, name = 'RF_Model',mode = 'lines+markers') %>%
  add_trace(y = ~LR_Model, name = 'LR_Model',mode = 'lines+markers', line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash'))


new_train <- miceOutput2
rm(miceOutput2)

new_train$price <- train$price

#Check for Missing values
missing_values <- new_train %>% summarize_all(funs(sum(is.na(.))/n()))
#missing_values <- test %>% summarize_all(funs(sum(is.na(.))/n()))

missing_values <- gather(missing_values, key="feature", value="missing_pct")
missing_values %>% 
  ggplot(aes(x=reorder(feature,-missing_pct),y=missing_pct)) +
  geom_bar(stat="identity",fill="red")+
  coord_flip()+theme_bw() + labs(x='features', y='% missing', title='Percent missing data by feature')


# #Replace missings with the related column means
# for (name in numeric_cols) {
#   train[[name]][is.na(train[[name]])] <-  mean(train[[name]], na.rm=TRUE)
# }

#exclude the price nas
new_train <- new_train[!is.na(new_train$price),]

nrow(new_train[is.na(new_train$price),])
#seperated the data
char_train <- new_train[, -which(names(new_train) %in% numerical)]
num_train <- new_train[, which(names(new_train) %in% numerical)]


#Categorical Variables

plotHist <- function(data_in, i) 
{
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data=data, aes(x=factor(x))) + stat_count() + xlab(colnames(data_in)[i]) + theme_light() + 
    theme(axis.text.x = element_text(angle = 90, hjust =1))
  return (p)
}

doPlots <- function(data_in, fun, ii, ncol=3) 
{
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

doPlots(char_train, fun = plotHist, ii = 1:6, ncol = 2)
doPlots(char_train, fun = plotHist, ii = 7:11, ncol = 2)
new_train[["num-of-doors"]][new_train[["num-of-doors"]] == "?"] <-  "four"



plotDen <- function(data_in, i){
  data <- data.frame(x=data_in[[i]])
  p <- ggplot(data = data) + geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab(paste0((colnames(data_in)[i]), '\n', 'Skewness: ',round(skewness(data_in[[i]], na.rm = TRUE), 2))) + theme_light() 
  return(p)
  
}


doPlots(num_train, fun = plotDen, ii = 1:8, ncol = 2)
doPlots(num_train, fun = plotDen, ii = 9:15, ncol = 2)

# #Box plot
# p <- ggplot(num_train, aes(x = char_train$make, num_train$price))
# 
# p + geom_boxplot(outlier.colour="red", outlier.shape=16,
#              outlier.size=2, notch=FALSE)
# 
# #Box plot
# p <- ggplot(num_train, aes(x = char_train$`fuel-system`, num_train$price))
# 
# p + geom_boxplot(outlier.colour="red", outlier.shape=16,
#                  outlier.size=2, notch=FALSE)
# 
# #Box plot
# p <- ggplot(num_train, aes(x = char_train$`engine-type`, num_train$price))
# 
# p + geom_boxplot(outlier.colour="red", outlier.shape=16,
#                  outlier.size=2, notch=FALSE)
# 
# #Box plot
# p <- ggplot(num_train, aes(x = char_train$`num-of-cylinders`, num_train$price))
# 
# p + geom_boxplot(outlier.colour="red", outlier.shape=16,
#                  outlier.size=2, notch=FALSE)
# 
# #Box plot
# p <- ggplot(num_train, aes(x = char_train$symboling, num_train$price))
# 
# p + geom_boxplot(outlier.colour="red", outlier.shape=16,
#                  outlier.size=2, notch=FALSE)


get_dup(train)

   ##### NICE DENSITY GRAPHS ----
#Categorical Variables
library(ggridges)

plotRidges <- function(data_in, i) 
{
  data <- data.frame(x = data_in[,i])
  colnames(data) <- names(data_in[i])
  #data[,i] <- as.factor(data[,i])
  data <- cbind(data, price = new_train$price)
  p <- ggplot(data, aes(x=price, y = data[[1]])) +  ylab(label = names(data_in[i])) +
    geom_density_ridges_gradient(
      aes(fill = ..x..), scale = 3, size = 0.3
    ) +
    scale_fill_gradientn(
      colours = c("#0D0887FF", "#CC4678FF", "#F0F921FF"),
      name = "Make"
    ) +
    labs(title = paste(label = names(data_in[i]), ' vs Price Distribution'))
  return (p)
}

doPlots <- function(data_in, fun, ii, ncol=3) 
{
  pp <- list()
  for (i in ii) {
    p <- fun(data_in=data_in, i=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

doPlots(char_train, fun = plotRidges, ii = 1:2, ncol = 2)
doPlots(char_train, fun = plotRidges, ii = 3:8, ncol = 2)
doPlots(char_train, fun = plotRidges, ii = 9:11, ncol = 2)


#-------Extract the columns has 0 variance
options(scipen = 99)
vars <- sapply(new_train[, which((names(new_train) %in% numerical))], function(x) var(x, na.rm  = T))
extract_vars <- rownames(data.frame(vars[which(vars < 0.01)]))
extract_vars
#train <- train[ , -which(names(train) %in% extract_vars)]

char_train <- new_train[, -which(names(new_train) %in% numerical)]
num_train <- new_train[, which(names(new_train) %in% numerical)]
#new_train <- train[!is.na(train$price),]
rm(miceMod2, missdata, missing_values,p)


#create correlation matrix, (model matrix creates a one hot encoding method)
m <- model.matrix(~.-1, num_train) %>% cor(method = "pearson")

corrplot(m, type = "upper", method="color", addCoef.col = "black", tl.cex = .7,cl.cex = .7, number.cex=.7)

cor_var <- findCorrelation(m, cutoff = 0.70, names = TRUE) %>% gsub("`", "", .)

#MULTICOLLINEARITY !

#extract highly correlated ones
new_train %<>% dplyr::select(-one_of(cor_var[cor_var != "price"]))


# library(GGally)
# 
# ggscatmat(num_train, columns = 1: ncol(num_train), corMethod = "pearson")

char_train <- new_train[, -which(names(new_train) %in% numerical)]
num_train <- new_train[, which(names(new_train) %in% numerical)]
new_train <- new_train[!is.na(new_train$price),]



#--------------------------MODEL PREPERATION------------------------
set.seed(1234)
parts <- createDataPartition(new_train$price, p = 0.8, list = F) %>% c()
train <- new_train
train[, -which(names(train) %in% numerical)] <- lapply(train[, -which(names(train) %in% numerical)], as.factor)
new_train <- train[parts,]
new_test <- train[-parts,]
price <- train$price


names <- names(new_train)
names <- gsub(x = names, "-", "")
colnames(new_train) <- names
colnames(new_test) <- names


#------------------------------XGB-------------------------------

new_train <- model.matrix(~.+0,new_train[,-c(20)]) 
new_test <- model.matrix(~.+0,data = new_test[,-c(20)])

xgbtest <- xgb.DMatrix(data = data.matrix(new_test))
xgbtrain <- xgb.DMatrix(data = data.matrix(new_train), label = price[parts])

## HYPERPARAMETER TUNING ------
All_rmse<- c()
Param_group<-c()
system.time(
  for (iter in 1:10) { #burda iteration sayısını arttırabilirz
    param <- list(objective = "reg:linear",
                  eval_metric = "rmse",
                  booster = "gbtree",
                  max_depth = sample(6:10, 1),
                  #Maximum depth of a tree. Increasing this value will make the model more complex and more likely to overfit.
                  eta = runif(1, 0.01, 0.3), 
                  # Step size shrinkage used in update to prevents overfitting. 
                  # After each boosting step, we can directly get the weights of new features, 
                  # and eta shrinks the feature weights to make the boosting process more conservative.
                  gamma = runif(1, 0.0, 0.2), 
                  # Minimum loss reduction required to make a further partition on a leaf node of the tree.
                  # controls my error reduction rate required to make a split.
                  # The larger gamma is, the more conservative the algorithm will be.
                  subsample = runif(1, 0.6, 0.9),
                  # Subsample ratio of the training instances. Setting it to 0.5 means that XGBoost
                  # would randomly sample half of the training data prior to growing trees. and this will prevent overfitting.
                  colsample_bytree = runif(1, 0.5, 0.8)
                  #Subsample ratio of columns when constructing each tree. 
                  
    )
    cv.nround = 100
    cv.nfold = 5
    mdcv <- xgb.cv(data=xgbtrain, params = param, nthread=10, #Number of parallel threads used to run XGBoost 
                   nfold=cv.nfold, nrounds=cv.nround,verbose = TRUE)
    # Least Mean_Test_RMSE as Indicator # 
    min_rmse <- min(mdcv$evaluation_log[,test_rmse_mean])
    All_rmse <- append(All_rmse, min_rmse)
    Param_group <- append(Param_group, param)
    # Select Param
    param <- Param_group[((which.min(All_rmse)-1)*8+1):((which.min(All_rmse)-1)*8+8)]
  }
)

# SORTING IS IMPORTANT for greedy algorithm to train lots of trees

### NUMBER OF ROUNDS TUNING ----
xgbcv <- xgb.cv(params = param, data = xgbtrain, nrounds = 300, nfold = 5,
                 showsd = T, stratified = T, print_every_n = 10, early_stopping_rounds = 20, maximize = F)

xgb1 <- xgb.train(params = param, data = xgbtrain, nrounds = 66, nthread = 6,verbose = T,
                   watchlist = list(train=xgbtrain), print_every_n = 10, early_stopping_rounds = 20, 
                   maximize = F )


prediction <- predict(xgb1, xgbtest)
preds_vs_actuals <- data.frame(cbind(prediction, price[-parts])) %>% cbind(error = prediction - price[-parts])

library(plotly)
xgb_graph <- plotly::plot_ly(preds_vs_actuals, y = prediction, mode = "lines", type = "scatter", name = "Prediction-xgb") %>% 
  add_trace(y = preds_vs_actuals$V2, type = "scatter", mode = "lines", name = "Actuals-xgb") %>%
  add_trace(y = preds_vs_actuals$error, type = "scatter", mode = "lines", name = "Error-xgb") %>% 
  layout(title = paste("R2 = ", round(R2_Score(preds_vs_actuals$prediction, preds_vs_actuals$V2),3), " and RMSE: ", round(sqrt(mean(preds_vs_actuals$error^2)),2)))
xgb_graph


## DENSITY PLOT -----
library(ggpubr)
a <- ggplot(preds_vs_actuals, aes(x = error))
a + geom_histogram(aes(y = ..density..), 
                   colour="black", fill="white") +
  geom_density(alpha = 0.2, fill = "#EFC000FF") 

##  QQPLOT -----
ggqqplot(preds_vs_actuals, x = "error",
         palette = c("#0073C2FF", "#FC4E07"),
         ggtheme = theme_pubclean())


#### IMPORTANCE PLOT ----- 
xgb.importance(names(xgb1$feature_names), model = xgb1) %>% xgb.plot.importance(top_n = 20)
### SEE THE TREE ----
xgb.plot.multi.trees(xgb1)

#perform t test to test the results
t.test(preds_vs_actuals$V2, preds_vs_actuals$prediction)

#MSE
#sqrt(mean(preds_vs_actuals$error^2))
