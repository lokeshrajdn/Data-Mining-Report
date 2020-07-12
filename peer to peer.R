
# Loading the packages ----------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load("ggplot2", "tidyverse","dplyr", "corrplot","ggcorrplot","Boruta","caret","e1071","randomForest","ROSE","class")

####################### Dataset2##############################################--
Peer.to.peer<-loan_final313
peer_df<-Peer.to.peer

# Understanding Data ------------------------------------------------------
head(peer_df)  # headers of the file
str(peer_df)  # Structure of Data
sapply(peer_df, FUN=function(x) {sum(is.na(x))}) # Missing Values Check

# Correlation -------------------------------------------------------------
#Building correlation plot among variables
numeric <- names(select_if(peer_df, is.numeric))
cor_df <- as.matrix(peer_df[numeric])
cor_mat <- cor(as.matrix(peer_df[numeric[-13]]))

#cor_mat
#Visualising the correlation matrix
corrplot(cor_mat, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

#############################################################
dataQualityNum <- function(df) {
  # filter numerics
  n <- sapply(df, function(x) {is.numeric(x)})
  df_numerics <- df[, n]
  # count number of rows
  instances <- sapply(df_numerics, FUN=function(x) {length(x)})
  # count number of missing values
  missing <- sapply(df_numerics, FUN=function(x) {sum(is.na(x))})
  missing <- missing / instances * 100
  # determine the length of the vector of unique values
  unique <- sapply(df_numerics, FUN=function(x) {length(unique(x))})
  # calculate the quantiles
  quantiles <- t(sapply(df_numerics, FUN=function(x) {quantile(x)}))
  # calculate the mean
  means <- sapply(df_numerics, FUN=function(x) {mean(x)})
  # calculate the standard deviation
  sds <- sapply(df_numerics, FUN=function(x) {sd(x)})
  # sapply is a loop operator that works on the COLUMNS of a data.frame
  # build a dataframe of all components of the DQR
  df_numeric <- data.frame(Feature=names(df_numerics),
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
  df_numerics <- dataQualityNum(peer_df)
#And show:
  df_numerics
  
  peer_dfID <- NULL
  
    hist(peer_df$annual_inc, main="Income")
    hist(peer_df$loan_amount, main="Loan Amount")
    hist(peer_df$purpose_cat, main="Purpose of loan")
    hist(peer_df$home_ownership_cat, main="Home ownership cat")
    hist(peer_df$emp_length_int, main="Employee experience")
   #####################################################################
    
    
    #Next a function for categorical data:
    dataQualityCat <- function(df) {
      # filter categoricals this time
      # here we assume only numeric or categorical data
      # essentially we say categoricals are NOT numeric
      n <- sapply(df, function(x) {is.numeric(x)})
      df_categoricals <- df[, !n]
      # as before
      instances <- sapply(df_categoricals, FUN=function(x) {length(x)})
      missing <- sapply(df_categoricals, FUN=function(x) {sum(is.na(x))})
      missing <- missing / instances * 100
      unique <- sapply(df_categoricals, FUN=function(x) {length(unique(x))})
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
      # now throw away the mode and repeat for the second mode
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
      df_categorical <- data.frame(Feature=names(df_categoricals),
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
    df_categorical <- dataQualityCat(peer_df)
    # And show:
    df_categorical
    # And show:
      df_categorical
      
      barplot(table(peer_df$region), main="REGION")
      barplot(table(peer_df$grade), main="GRADE")
      barplot(table(peer_df$purpose), main="PURPOSE")
      barplot(table(peer_df$loan_condition), main="LOAN CONDITION")
      barplot(table(peer_df$home_ownership), main="LOAN CONDITION")
      
      boxplot(installment~region,data=peer_df, main="REGION")
      boxplot(annual_inc~region,data=peer_df, main="REGION")
      
      boxplot(emp_length_int~income_cat,data=peer_df, main="exprience vs income cat")
     
##########################################################################################      

ggplot(aes(year,emp_length_int,color=home_ownership_cat),data=peer_df)+geom_point() +
  ggtitle("Employee exp Vs Years")+
  xlab("years")+
  ylab("Employee Experince")
######################################################################################      
      
# Model Building ----------------------------------------------------------
View(peer_df)
set.seed(123)
my_df  <- (peer_df[numeric])
my_df <- sample_n(my_df,10000)

my_df$loan_condition_cat <- as.factor(my_df$loan_condition_cat)
splitIndex <- createDataPartition(my_df$loan_condition_cat, p=.70, list=FALSE, times=1)
train <- my_df[splitIndex, ]
test <- my_df[-splitIndex, ]

      
# Logisitic Regression ----------------------------------------------------
baseline_model <- glm(loan_condition_cat ~ ., data = subset(train,select=c(-id,-final_d)), family=binomial)
summary(baseline_model)

test$pred <- as.factor(ifelse(predict(baseline_model,newdata = test) >0.5, 0,1))
confusionMatrix(test$loan_condition_cat,test$pred, positive = "1")


# Random Forest -----------------------------------------------------------
rf_model <- randomForest(loan_condition_cat ~ ., data = subset(train,select=c(-id,-final_d)))
summary(baseline_model)
varImpPlot(rf_model)

test$pred2 <- (predict(rf_model,newdata = test))
confusionMatrix(test$loan_condition_cat,test$pred2, positive = "1")


########################--Over sampling---#################################
#Over Estimating Positives
over <- ovun.sample(loan_condition_cat~., data = train, method = "over",N = 12940)$data
table(over$loan_condition_cat)

#Using base line model on Over data
over_model <- glm(loan_condition_cat ~ ., data = subset(over,select=c(-id,-final_d)), family=binomial)
summary(over_model)

  

#Using RF model on over data
rf_over_model <- randomForest(loan_condition_cat ~ ., data = subset(over,select=c(-id,-final_d)))
                                                                                                                                                                                                                                

test$pred4 <- (predict(rf_over_model,newdata = test))
confusionMatrix(test$loan_condition_cat,test$pred4, positive = "1")

##########################SVM####################################
library(kernlab)
svm4 <- ksvm(loan_condition_cat ~ ., data = train)
svm4.pred <- predict(svm4, newdata = test, type = 'response')
# confusion matrix 
confusionMatrix(svm4.pred, test$loan_condition_cat)
                                                                                                                                                                                                                                                                                                                              