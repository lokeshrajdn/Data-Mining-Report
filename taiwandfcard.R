# Loading The Dataset -----------------------------------------------------
##############################Data Set 2####################################

Credit_df1<- Taiwan.credit
View(Credit_df1)

# Understanding Data ------------------------------------------------------
head(Credit_df1)  # headers of the file
str(Credit_df1)  # Structure of Data
sapply(Credit_df1, FUN=function(x) {sum(is.na(x))}) # Missing Values Check

######################## Correlation ###############################################
#Building correlation plot among variables
numeric <- names(select_if(Credit_df1, is.numeric))
cor_df <- as.matrix(Credit_df1[numeric])
cor_mat <- cor(as.matrix(cor_df))

#cor_mat
#Visualising the correlation matrix
corrplot(cor_mat, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
#####################DQR#################################################################
dataQualityNum <- function(df) {
  # filter numerics
  n <- sapply(df, function(x) {is.numeric(x)})
  df_numerics1 <- df[, n]
  # count number of rows
  instances <- sapply(df_numerics1, FUN=function(x) {length(x)})
  # count number of missing values
  missing <- sapply(df_numerics1, FUN=function(x) {sum(is.na(x))})
  missing <- missing / instances * 100
  # determine the length of the vector of unique values
  unique <- sapply(df_numerics1, FUN=function(x) {length(unique(x))})
  # calculate the quantiles
  quantiles <- t(sapply(df_numerics1, FUN=function(x) {quantile(x)}))
  # calculate the mean
  means <- sapply(df_numerics1, FUN=function(x) {mean(x)})
  # calculate the standard deviation
  sds <- sapply(df_numerics1, FUN=function(x) {sd(x)})
  # sapply is a loop operator that works on the COLUMNS of a data.frame
  # build a dataframe of all components of the DQR
  df_numeric <- data.frame(Feature=names(df_numerics1),
                           Instances=instances,
                           Missing=missing,
                           Cardinality=unique,
                           Min=quantiles[,1],
                           FirstQuartile=quantiles[,2],
                           Median=quantiles[,3],
                           ThirdQuartile=quantiles[,4],
                           Max=quantiles[,5],
                           Mean=means,
                           Stdev=sds)
  #  remove rownames -- they have no meaning here
  rownames(df_numeric) <- NULL
  return(df_numeric)
}
#Now to call:
df_numerics1 <- dataQualityNum(Credit_df1)
#And show:
df_numerics1

Credit_df1ID <- NULL

hist(Credit_df1$LIMIT_BAL, main="Limit")
hist(Credit_df1$AGE, main="AGE")
hist(Credit_df1$LIMIT_BAL, main="LIMIT_BAL")
hist(Credit_df1$default.payment.next.month, main="DEFAULT PAYMENT NEXT MONTH")
#####################################################################

## Fixing cATEGORICAL VAIABLES###############################
Credit_df1$SEX <- as.factor(Credit_df1$SEX)
Credit_df1$EDUCATION <- as.factor(Credit_df1$EDUCATION)
Credit_df1$MARRIAGE <- as.factor(Credit_df1$MARRIAGE)

str(Credit_df1)
#############################################DQR CATEGORICAL#################################
dataQualityCat <- function(df) {
  # filter categoricals this time
  # here we assume only numeric or categorical data
  # essentially we say categoricals are NOT numeric
  n <- sapply(df, function(x) {is.numeric(x)})
  df_categoricals <- Credit_df1[, !n]
  # as before
  instances <- sapply(df_categoricals, FUN=function(x) {length(x)})
  missing <- sapply(df_categoricals, FUN=function(x) {sum(is.na(x))})
  missing <- missing / instances * 100
  unique <- sapply(df_categoricals, FUN=function(x) {length(unique(x))})
  # find the most frequent categorical level
  modeFreqs <- sapply(df_categoricals, FUN=function(x) {
    t <- table(x)
    modeFreq <- max(t)
    return(modeFreq)
  })
  # for all modes, get their frequency
  modes <- sapply(df_categoricals, FUN=function(x) {
    t <- table(x)
    modeFreq <- max(t)
    mode <- names(t)[t==modeFreq]
    return(mode)
  })
  #now throw away the mode and repeat for the second mode
  modeFreqs2 <- sapply(df_categoricals, FUN=function(x) {
    t <- table(x)
    modeFreq <- max(t)
    mode <- names(t)[t==modeFreq]
    # we remove the 1st mode here
    x <- x[x != mode]
    t <- table(x)
    mode2Freq <- max(t)
    return(mode2Freq)
  })
  modes2 <- sapply(df_categoricals, FUN=function(x) {
    t <- table(x)
    modeFreq <- max(t)
    mode <- names(t)[t==modeFreq]
    # we remove the 1st mode here
    x <- x[x != mode]
    t <- table(x)
    mode2Freq <- max(t)
    mode2 <- names(t)[t==mode2Freq]
    return(mode2)
  })
  # build data.frame as before, but also derive the mode frequenies
  df_categorical <- data.frame(
    Instances=instances,
    Missing=missing,
    Cardinality=unique,
    FirstMode=modes,
    FirstModeFreq=modeFreqs,
    FirstModePercent=modeFreqs/instances*100,
    SecondMode=modes2,
    SecondModeFreq=modeFreqs2,
    SecondModePercent=modeFreqs2/instances*100)
  rownames(df_categorical) <- NULL
  return(df_categorical)
}
# Now to call:
df_categorical <- dataQualityCat(Credit_df1)
# And show:
df_categorical

barplot(table(Credit_df1$SEX), main="SEX")
barplot(table(Credit_df1$MARRIAGE ), main="Marriage")
#######################################################################



# Model Building ----------------------------------------------------------
View(Credit_df1)
set.seed(123)
my_df  <- (Credit_df1[numeric])
my_df <- sample_n(my_df,10000)

my_df$default.payment.next.month <- as.factor(my_df$default.payment.next.month)
splitIndex <- createDataPartition(my_df$default.payment.next.month, p=.70, list=FALSE, times=1)
train <- my_df[splitIndex, ]
test <- my_df[-splitIndex, ]


# Logisitic Regression ----------------------------------------------------
baseline_model <- glm(default.payment.next.month ~ ., data = train,  family=binomial)
summary(baseline_model)

test$pred <- as.factor(ifelse(predict(baseline_model,newdata = test) >0.5, 0,1))
confusionMatrix(test$default.payment.next.month,test$pred, positive = "1")


# Random Forest -----------------------------------------------------------
rf_model <- randomForest(default.payment.next.month ~ ., data = train)
summary(baseline_model)
varImpPlot(rf_model)

test$pred2 <- (predict(rf_model,newdata = test))
confusionMatrix(test$default.payment.next.month,test$pred2, positive = "1")

# Class imbalance problem selection -----------------------------------------------


#Over Estimating Positives
colnames(train)[25] <- "y_cat"
over <- ovun.sample(y_cat~., data = train, method = "over",N = 10000)$data
table(over$`y_cat`)

#Using base line model on Over data
over_model <- glm(y_cat ~ ., data = over, family=binomial)
summary(over_model)

test$pred3 <- as.factor(ifelse(predict(over_model,newdata = test) >0.5, 0,1))

confusionMatrix(test$default.payment.next.month,test$pred3, positive = "1")

#Using RF model on over data
rf_over_model <- randomForest(y_cat ~ ., data = over,select=c(-id))
summary(rf_over_model)

test$pred4 <- (predict(rf_over_model,newdata = test))
confusionMatrix(test$default.payment.next.month,test$pred4, positive = "1")
##########################SVM####################################
library(kernlab)
svm4 <- ksvm(y_cat ~ ., data = train)
svm4.pred <- predict(svm4, newdata = test, type = 'response')
# confusion matrix 
confusionMatrix(svm4.pred, test$default.payment.next.month)
