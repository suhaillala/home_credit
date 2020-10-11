# Install packages
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("forecast")
install.packages("gplots")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("pRoC")
install.packages("lattice")
install.packages("caret")
install.packages("e1071")
install.packages("gains")
install.packages("ROCR")

# Initial load
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyr)
library(forecast)
library(gplots)
library(rpart)
library(rpart.plot)
library(pROC)
library(lattice)
library(caret)
library(e1071)
library(gains)
library(ROCR)

# Initializing variables
hc.df <- read.csv("application_train.csv")
predictors <- list()
class.model <- list()
reg.model <- list()
fit.accr <- list()
plots <- list()
clusters <- 5


#####################
#                   #
#   Data Selection  #
#                   #
#####################

# Data understanding
summary(hc.df)
t(names(hc.df))

# Assign customer Ids to row names
row.names(hc.df) <- hc.df$SK_ID_CURR
hc.df <- hc.df[, -match("SK_ID_CURR", colnames(hc.df))]

# Remove columns if N.A. > 30% of total observations
for (colname in colnames(hc.df)) {
  if (mean(is.na(hc.df[, colname])) > 0.3) {
    hc.df <- hc.df[, -match(colname, colnames(hc.df))]
  }
}
t(names(hc.df))

### Determine significant predictors
# Classification tree
class.model[["CSTREE"]] <- rpart(TARGET ~ ., data = hc.df, method = "class", cp = 0.000001, minsplit = 2)
class.model[["CSTREE"]]$variable.importance

# Regression tree
reg.model[["REGTREE"]] <- rpart(AMT_CREDIT ~ ., data = hc.df[hc.df$TARGET == 0,], cp = 0.000001, minsplit = 2)
reg.model[["REGTREE"]]$variable.importance

### Select significant variables + TARGET
predictors[["CATEGORICAL"]] <- c("CODE_GENDER",
                                 "NAME_EDUCATION_TYPE",
                                 "NAME_FAMILY_STATUS",
                                 "OCCUPATION_TYPE",
                                 "REGION_RATING_CLIENT",
                                 "REG_CITY_NOT_LIVE_CITY",
                                 "FLAG_OWN_CAR",
                                 "FLAG_PHONE",
                                 "TARGET")

predictors[["NUMERICAL"]] <- c("AMT_CREDIT",
                               "AMT_ANNUITY",
                               "AMT_GOODS_PRICE",
                               "REGION_POPULATION_RELATIVE",
                               "DAYS_BIRTH",
                               "DAYS_EMPLOYED",
                               "EXT_SOURCE_2",
                               "EXT_SOURCE_3",
                               "OBS_30_CNT_SOCIAL_CIRCLE",
                               "AMT_REQ_CREDIT_BUREAU_YEAR",
                               "AMT_INCOME_TOTAL",
                               "CNT_CHILDREN")

predictors[["ALL"]] <- c(predictors[["CATEGORICAL"]], predictors[["NUMERICAL"]])

hc.df <- hc.df[, predictors[["ALL"]]]
summary(hc.df)
t(names(hc.df))



################
#              #
#   Anomalies  #
#              #
################

#DAYS_BIRTH

summary(as.data.frame(hc.df$DAYS_BIRTH/-365)) #Convert negative values to readable form

#DAYS EMPLOYED

t(t(summary(hc.df$DAYS_EMPLOYED))) #Anamoly detected
hist(hc.df$DAYS_EMPLOYED) #Plot histogram to see frequency of anomalies
sum(hc.df$DAYS_EMPLOYED>3417.5) #55374 records with 1000 years of work

#Check how much percentage of anomalies and non anomalies defaulting loans
sprintf("There are %s anomalous records of employment", 
        nrow(hc.df[hc.df$DAYS_EMPLOYED==365243,]))
sprintf("The non-anomalies default on %s percent of loans", 
        round(100*mean(hc.df[hc.df$DAYS_EMPLOYED!=365243, "TARGET"]), digit=3))
sprintf("The anomalies default on %s percent of loans", 
        round(100*mean(hc.df[hc.df$DAYS_EMPLOYED==365243, "TARGET"]), digit=3))


#Anomalies have low rate of defaulting; therefore, their data is valuable. One approach is to remove extreme values, and impute the median of that column 
#Replace 365243 records with NA; plot histogram to see data distribution

#replace values with NA (dplyr)

hc.df$DAYS_EMPLOYED <- na_if(hc.df$DAYS_EMPLOYED,365243)
is.na(hc.df$DAYS_EMPLOYED) #Check NA values

hist(hc.df$DAYS_EMPLOYED)

#replace NAs with values (tidyr)

hc.df$DAYS_EMPLOYED <- replace_na(
  hc.df$DAYS_EMPLOYED, 
  median(hc.df$DAYS_EMPLOYED, na.rm=TRUE)) #Replace NA with median
is.na(hc.df$DAYS_EMPLOYED)

hist(ifelse(hc.df$DAYS_EMPLOYED<0,hc.df$DAYS_EMPLOYED/-365,hc.df$DAYS_EMPLOYED/365), xlab = "YEARS EMPLOYED") #distrubtion positive-skew



################
#              #
#   Outliers   #
#              #
################

# Create boxplots
for (colname in predictors[["NUMERICAL"]]) {
  plots[[colname]] <- ggplot(hc.df, aes_string(y = colname)) + 
    geom_boxplot(outlier.color = "red",
                 outlier.shape = 24,
                 outlier.fill = "red",
                 outlier.size = 3,
                 na.rm = TRUE) +
    xlab(colname) + ylab("")
}
options(scipen = 5)
ggarrange(plotlist = plots)


# Remove extreme outliers
hc.df <- hc.df[-union_all(
  which(hc.df$REGION_POPULATION_RELATIVE == max(hc.df$REGION_POPULATION_RELATIVE, na.rm = TRUE),
        hc.df$REGION_POPULATION_RELATIVE),
  which(hc.df$OBS_30_CNT_SOCIAL_CIRCLE == max(hc.df$OBS_30_CNT_SOCIAL_CIRCLE, na.rm = TRUE),
        hc.df$OBS_30_CNT_SOCIAL_CIRCLE),
  which(hc.df$AMT_INCOME_TOTAL == max(hc.df$AMT_INCOME_TOTAL, na.rm = TRUE), hc.df$AMT_INCOME_TOTAL)),]


# Create new boxplots
plots <- list()
for (colname in predictors[["NUMERICAL"]]) {
  plots[[colname]] <- ggplot(hc.df, aes_string(y = colname)) + 
    geom_boxplot(outlier.color = "red",
                 outlier.shape = 24,
                 outlier.fill = "red",
                 outlier.size = 3,
                 na.rm = TRUE) +
    xlab(colname) + ylab("")
}
ggarrange(plotlist = plots)
plots <- NULL



#####################
#                   #
#   Missing Values  #
#                   #
#####################

summary(hc.df)
#5 columns containing missing values
#replacing NA values with medians

for (colname in predictors[["NUMERICAL"]]) {
  hc.df[is.na(hc.df[,colname]), colname] <- median(hc.df[, colname], na.rm = TRUE)
}

summary(hc.df) # No missing values


########################
#                      #
#   Multicollinearity  #
#                      #
########################

# Check for multicollinearity
corr <- cor(cbind(hc.df[, predictors[["NUMERICAL"]]],  ## corr stores correlation
                  as.data.frame(model.matrix(~ ., data = hc.df[, predictors[["CATEGORICAL"]]]))))
corr <- corr[-match("(Intercept)", row.names(corr)),-match("(Intercept)", colnames(corr))] #Remove intercept
heatmap.2(corr, Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
        notecol = "black", key = TRUE, trace = 'none')

# Print pairs of predictors with strong correlation (corr > |0.65|)
for (i in 1:nrow(corr)){
  correlations <-  which((abs(corr[i,]) > 0.65) & (corr[i,] != 1))
  
  if(length(correlations)> 0){
    print(colnames(corr)[i])
    print(correlations)
  }
}

# Remove variables with high correlation
predictors[["NUMERICAL"]] <- predictors[["NUMERICAL"]][-match(c(
  "AMT_ANNUITY", "AMT_GOODS_PRICE"),predictors[["NUMERICAL"]])]
predictors[["ALL"]] <- c(predictors[["NUMERICAL"]], predictors[["CATEGORICAL"]])
hc.df <- hc.df[, predictors[["ALL"]]]


# Check for multicollinearity again
corr <- cor(cbind(hc.df[, predictors[["NUMERICAL"]]], 
                  as.data.frame(model.matrix(~ ., data = hc.df[, predictors[["CATEGORICAL"]]]))))
corr <- corr[-match("(Intercept)", row.names(corr)),-match("(Intercept)", colnames(corr))] #Remove intercept
heatmap.2(corr, Rowv = FALSE, Colv = FALSE, dendrogram = "none",
          notecol = "black", key = TRUE, trace = 'none')

# Print pairs of predictors with strong correlation (corr > |0.65|)
for (i in 1:nrow(corr)){
  correlations <-  which((abs(corr[i,]) > 0.65) & (corr[i,] != 1))
  
  if(length(correlations)> 0){
    print(colnames(corr)[i])
    print(correlations)
  }
}


########################
#                      #
#   Describe Data      #
#                      #
########################

## Overall dataset
summary(hc.df)
str(hc.df)
dim(hc.df)
nrow(hc.df)
head(hc.df)
t(names(hc.df))

##############################################################################################################################

## Outcome = TARGET variable

# Describe data showing distribution of loan acceptance in categorical variables
plots[[length(plots) + 1]] <- ggplot(hc.df, aes(x=as.character(TARGET))) + geom_bar()+
  facet_grid(.~CODE_GENDER)+ggtitle("Loan Acceptance by Gender")+xlab("TARGET")
plots[[length(plots) + 1]]<-ggplot(hc.df, aes(x=as.character(TARGET)))+geom_bar()+
  facet_grid(.~FLAG_OWN_CAR)+ggtitle("Loan Acceptance by Car ownership")+xlab("TARGET")
ggarrange(plotlist = plots)
plots <- NULL

plots[[length(plots) + 1]]<-ggplot(hc.df, aes(x=as.character(TARGET)))+geom_bar()+
  facet_grid(.~REGION_RATING_CLIENT)+ggtitle("Loan Acceptance by region rating")+xlab("TARGET")
plots[[length(plots) + 1]]<-ggplot(hc.df, aes(x=as.character(TARGET)))+geom_bar()+
  facet_grid(.~REG_CITY_NOT_LIVE_CITY)+ggtitle("Loan Acceptance by Diferrent address")+xlab("TARGET")
ggarrange(plotlist = plots)
plots <- NULL

ggplot(hc.df, aes(x=as.character(TARGET)))+geom_bar(aes(fill=OCCUPATION_TYPE),position="dodge")+ggtitle("Loan Acceptance by Occupation")+xlab("TARGET")
ggplot(hc.df, aes(x=as.character(TARGET)))+geom_bar()+facet_grid(.~NAME_EDUCATION_TYPE)+ggtitle("Loan Acceptance by Education")+xlab("TARGET")
ggplot(hc.df, aes(x=as.character(TARGET)))+geom_bar()+facet_grid(.~NAME_FAMILY_STATUS)+ggtitle("Loan Acceptance by Family status")+xlab("TARGET")
ggplot(hc.df, aes(x=as.character(TARGET)))+geom_bar()+facet_grid(.~FLAG_PHONE)+ggtitle("Loan Acceptance by Mobile")+xlab("TARGET")

# Describe data showing loan acceptance for numerical variable
plots[[length(plots) + 1]]<-ggplot(hc.df) + 
  geom_bar(aes(x = as.character(TARGET), y = EXT_SOURCE_2, fill = NAME_EDUCATION_TYPE), show.legend = FALSE,
           position = "dodge",stat = "summary", fun.y = "mean") + 
  xlab("TARGET") + ylab("External Score2")+ggtitle("Loan Acceptance by External Score2")
plots[[length(plots) + 1]]<-ggplot(hc.df) + 
  geom_bar(aes(x = as.character(TARGET), y = EXT_SOURCE_3, fill = NAME_EDUCATION_TYPE), show.legend = TRUE,
           position = "dodge",stat = "summary", fun.y = "mean") + 
  xlab("TARGET") + ylab("External Score3")+ggtitle("Loan Acceptance by External Score3")
ggarrange(plotlist = plots, common.legend = TRUE, legend = "right")
plots <- NULL

plots[[length(plots) + 1]]<-ggplot(hc.df) + 
  geom_bar(aes(x = as.character(TARGET), y = DAYS_EMPLOYED/-1, fill = CODE_GENDER), position = "dodge",
           stat = "summary", fun.y = "mean") + 
  xlab("TARGET") + ylab("Days Employed")+ggtitle("Loan Acceptance by:")
plots[[length(plots) + 1]]<-ggplot(hc.df) + 
  geom_bar(aes(x = as.character(TARGET), y = DAYS_BIRTH/-1, fill = CODE_GENDER), position = "dodge",
           stat = "summary", fun.y = "mean") + 
  xlab("TARGET") + ylab("Age")+ggtitle("Loan Acceptance by:")
plots[[length(plots) + 1]]<-ggplot(hc.df) + 
  geom_bar(aes(x = as.character(TARGET), y = AMT_CREDIT, fill = CODE_GENDER), position = "dodge",
           stat = "summary", fun.y = "mean") + 
  xlab("TARGET") + ylab("Loan Amount")+ggtitle("Loan Acceptance by:")
ggarrange(plotlist = plots, common.legend = TRUE, legend = "right", nrow = 1, ncol = 3)
plots <- NULL


plots[[length(plots) + 1]]<-ggplot(hc.df) + 
  geom_bar(aes(x = as.character(TARGET), y = AMT_CREDIT, fill = OCCUPATION_TYPE), position = "dodge",
           stat = "summary", fun.y = "mean") + 
  xlab("TARGET") + ylab("Credit Amount")+ggtitle("Loan Acceptance by Credit amount")
plots[[length(plots) + 1]]<-ggplot(hc.df) + 
  geom_bar(aes(x = as.character(TARGET), y = AMT_INCOME_TOTAL, fill = OCCUPATION_TYPE), position = "dodge",
           stat = "summary", fun.y = "mean") + 
  xlab("TARGET") + ylab("Income")+ggtitle("Loan Acceptance by Income")
ggarrange(plotlist = plots, common.legend = TRUE, legend = "right")
plots <- NULL

plots[[length(plots) + 1]]<-ggplot(hc.df) + 
  geom_bar(aes(x = as.character(TARGET), y = CNT_CHILDREN, fill = NAME_FAMILY_STATUS), position = "dodge",
           stat = "summary", fun.y = "mean") + 
  xlab("TARGET") + ylab("Number of kids")+ggtitle("Loan Acceptance by number of Kids")
plots[[length(plots) + 1]]<-ggplot(hc.df) + 
  geom_bar(aes(x = as.character(TARGET), y = AMT_REQ_CREDIT_BUREAU_YEAR, fill = NAME_FAMILY_STATUS), position = "dodge",
           stat = "summary", fun.y = "mean") + 
  xlab("TARGET") + ylab("Credit Bureau Enquiries")+ggtitle("Loan Acceptance by Bureau Enquiries")
plots[[length(plots) + 1]]<-ggplot(hc.df) + 
  geom_bar(aes(x = as.character(TARGET), y = REGION_POPULATION_RELATIVE, fill = NAME_FAMILY_STATUS), position = "dodge",
           stat = "summary", fun.y = "mean") + 
  xlab("TARGET") + ylab("Region population density")+ggtitle("Loan Acceptance by Region Population")
ggarrange(plotlist = plots, common.legend = TRUE, legend = "right")
plots <- NULL

ggplot(hc.df) + 
  geom_bar(aes(x = as.character(TARGET), y = OBS_30_CNT_SOCIAL_CIRCLE, fill = OCCUPATION_TYPE), position = "dodge",
           stat = "summary", fun.y = "mean") + 
  xlab("TARGET") + ylab("30dpd observations")+ggtitle("Loan Acceptance by Region Default Rate")

ggplot(hc.df) + 
  geom_bar(aes(x = as.character(TARGET), y = AMT_CREDIT, fill = NAME_FAMILY_STATUS), position = "dodge",
           stat = "summary", fun.y = "mean") + 
  xlab("TARGET") + ylab("30dpd observations")+ggtitle("Loan Acceptance by Region Default Rate")

##############################################################################################################################

## Outcome = AMT_CREDIT variable

# Create new data frame with Target = 0
ac.df <- hc.df[hc.df$TARGET == 0,]  ## ac stands for AMT_CREDIT
ac.df <- ac.df[, -match("TARGET", colnames(ac.df))]

# Histogram of AMT_CREDIT
ggplot(ac.df, aes(x = AMT_CREDIT)) + 
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=130500,
                 colour="black", fill="white") +
  xlab("Credit Amount") + ylab("Density") +
  ggtitle("Distribution of Credit Amount")

## Effect of predictors on average credit amount
ggplot(ac.df) +
  geom_bar(aes(x = NAME_EDUCATION_TYPE, y = AMT_CREDIT, fill = CODE_GENDER),
           position = "dodge",
           stat ="summary", fun.y = "mean") +
  xlab("Education") + ylab("Average Credit") +ggtitle("Effect of education and gender on loan amount")

ggplot(ac.df) +
  geom_bar(aes(x = NAME_FAMILY_STATUS, y = AMT_CREDIT, fill = CODE_GENDER),
           position = "dodge",
           stat ="summary", fun.y = "mean") +
  xlab("Family Status") + ylab("Average Credit") +ggtitle("Effect of family status and gender on loan amount")

ggplot(ac.df) +
  geom_bar(aes(x = REGION_RATING_CLIENT, y = AMT_CREDIT, fill = OCCUPATION_TYPE),
           position = "dodge",
           stat ="summary", fun.y = "mean") +
  xlab("Region Rating Client") + ylab("Average Credit")+
  ggtitle("Effect of region rating and occupation on loan amount")

ggplot(ac.df) +
  geom_bar(aes(x = as.character(REGION_RATING_CLIENT), y = AMT_CREDIT, fill = FLAG_OWN_CAR),
           position = "dodge",
           stat ="summary", fun.y = "mean") +
  xlab("Rating of the region where client lives") + ylab("Average Credit")+
  ggtitle("Effect of region rating and car ownership on loan amount")

ggplot(ac.df) +
  geom_bar(aes(x = as.character(REG_CITY_NOT_LIVE_CITY), y = AMT_CREDIT, fill = CODE_GENDER),
           position = "dodge",
           stat ="summary", fun.y = "mean") +
  xlab("Reg City Not Live City") + ylab("Average Credit")

ggplot(ac.df) +
  geom_bar(aes(x = NAME_EDUCATION_TYPE, y = AMT_CREDIT, fill = FLAG_OWN_CAR),
           position = "dodge",
           stat ="summary", fun.y = "mean") +
  xlab("Education") + ylab("Average Credit")+
  ggtitle("Effect of education and car ownership on loan amount")

ggplot(ac.df) +
  geom_bar(aes(x = REGION_RATING_CLIENT, y = AMT_CREDIT, fill = NAME_EDUCATION_TYPE),
           position = "dodge",
           stat ="summary", fun.y = "mean") +
  xlab("Rating of the region where client lives") + ylab("Average Credit")+
  ggtitle("Effect of region rating and education on loan amount")



########################
#                      #
#     Clustering       #
#                      #
########################

# Create dummy variables
hc.dummy <- as.data.frame(model.matrix(~ ., data = hc.df[,predictors[["CATEGORICAL"]]]))
hc.dummy <- hc.dummy[,-match("(Intercept)", colnames(hc.dummy))] #Remove intercept

### Analyse hierarchial clusters (Requires RAM > 16 GB)
# Average Method
plot(hclust(dist(sample(sapply(cbind(
  hc.df[,predictors[["NUMERICAL"]]], hc.dummy), scale), dim(hc.df)[1]*0.1),
  method = "euclidean"), method = "average"), hang = -1, ann = FALSE)
gc() #Garbage collection

# Complete linkage Method
plot(hclust(dist(sample(sapply(cbind(
  hc.df[,predictors[["NUMERICAL"]]], hc.dummy), scale), dim(hc.df)[1]*0.1),
  method = "euclidean"), method = "complete"), hang = -1, ann = FALSE)
gc() #Garbage collection

# Single linkage Method
plot(hclust(dist(sample(sapply(cbind(
  hc.df[,predictors[["NUMERICAL"]]], hc.dummy), scale), dim(hc.df)[1]*0.1),
  method = "euclidean"), method = "single"), hang = -1, ann = FALSE)
gc() #Garbage collection

# Kmeans
set.seed(2)
km <- kmeans(sapply(cbind(hc.df[,predictors[["NUMERICAL"]]], hc.dummy), scale), clusters)
km$withinss
km$size
km$withinss / km$size   # Average sum of squares

# Add cluster information to ac.df
hc.df$CLUSTER <- as.character(km$cluster)

# Plot heatmap 
heatmap.2(km$centers, Rowv = FALSE, Colv = FALSE, dendrogram = "none", 
          notecol = "black", key = TRUE, trace = 'none')

# Centroids
km$centers

## Loan acceptance per cluster
ggplot(hc.df) + 
  geom_bar(aes(x = as.character(TARGET), fill = CLUSTER), position = "dodge") + 
  xlab("TARGET") + ylab("Number of applications accepted")+ggtitle("Loan Acceptance by Cluster")

## Average loan amount per cluster
ggplot(hc.df) + 
  geom_bar(aes(x = CLUSTER, y = AMT_CREDIT), position = "dodge", stat = "summary", fun.y = "mean") + 
  xlab("Cluster") + ylab("Average loan amount")+ggtitle("Average loan amount by Cluster")

## Analyse trend in clusters by loan amount
plots[[length(plots) + 1]] <- ggplot(hc.df) + 
  geom_smooth(aes(x = AMT_INCOME_TOTAL, y = AMT_CREDIT, color = CLUSTER), se = FALSE) +
  xlab("Total Income") + ylab("Average Credit Amount") +
  ggtitle("Total Income vs Average Credit Amount")

plots[[length(plots) + 1]] <- ggplot(hc.df) + 
  geom_smooth(aes(x = DAYS_EMPLOYED, y = AMT_CREDIT, color = CLUSTER), se = FALSE) +
  xlab("Days Employed") + ylab("Average Credit Amount") +
  ggtitle("Days Employed vs Average Credit Amount")
ggarrange(plotlist = plots, nrow = 2, ncol = 1)
plots <- NULL

plots[[length(plots) + 1]] <- ggplot(hc.df) + 
  geom_smooth(aes(x = EXT_SOURCE_2, y = AMT_CREDIT, color = CLUSTER), se = FALSE) +
  xlab("External Source 2") + ylab("Average Credit Amount") +
  ggtitle("External Source 2 vs Average Credit Amount")

plots[[length(plots) + 1]] <- ggplot(hc.df) + 
  geom_smooth(aes(x = EXT_SOURCE_3, y = AMT_CREDIT, color = CLUSTER), se = FALSE) +
  xlab("External Source 3") + ylab("Average Credit Amount") +
  ggtitle("External Source 3 vs Average Credit Amount")
ggarrange(plotlist = plots, nrow = 2, ncol = 1)
plots <- NULL

ggplot(hc.df) + 
  geom_smooth(aes(x = DAYS_BIRTH, y = AMT_CREDIT, colour = CLUSTER), se = FALSE) +
  xlab("Days Birth") + ylab("Average Credit Amount") +
  ggtitle("Days Birth vs Average Credit Amount")

ggplot(hc.df) + 
  geom_smooth(aes(x = REGION_POPULATION_RELATIVE, y = AMT_CREDIT, colour = CLUSTER), se = FALSE) +
  xlab("Region Population Relative") + ylab("Average Credit Amount") +
  ggtitle("Region Population Relative vs Average Credit Amount")

ggplot(hc.df) + 
  geom_smooth(aes(x = OBS_30_CNT_SOCIAL_CIRCLE, y = AMT_CREDIT, colour = CLUSTER), se = FALSE) +
  xlab("Number of observations defaulted") + ylab("Average Credit Amount") +
  ggtitle("Number of observations defaulted vs Average Credit Amount")

ggplot(hc.df) + 
  geom_smooth(aes(x = AMT_REQ_CREDIT_BUREAU_YEAR, y = AMT_CREDIT, colour = CLUSTER), se = FALSE) +
  xlab("Number of enquiries") + ylab("Average Credit Amount") +
  ggtitle("Number of enquiries to credit bureau vs Average Credit Amount")


########################
#                      #
#       Modeling       #
#                      #
########################

## Predicting AMT_CREDIT (how much loan to give)

# Create dummy variables
ac.dummy <- as.data.frame(model.matrix(~ ., data = ac.df[,setdiff(predictors[["CATEGORICAL"]], "TARGET")]))
ac.dummy <- ac.dummy[,-match("(Intercept)", colnames(ac.dummy))] #Remove intercept

# Initialization
class.model <- list()
reg.model <- list()
set.seed(7)
train.rows <- sample(rownames(ac.df), dim(ac.df)[1]*0.6)
valid.rows <- setdiff(row.names(ac.df), train.rows)
reg.train.df <- cbind(ac.df[train.rows,predictors[["NUMERICAL"]]], CLUSTER = hc.df[train.rows, "CLUSTER"],
                      ac.dummy[train.rows,])
reg.valid.df <- cbind(ac.df[valid.rows,predictors[["NUMERICAL"]]], CLUSTER = hc.df[valid.rows, "CLUSTER"],
                      ac.dummy[valid.rows,])


### Model development & prediction
## Linear regression
linear.pred <- list()
for (i in 1:clusters) {
  model.df <- reg.train.df[reg.train.df$CLUSTER == as.character(i),]
  model.df <- model.df[, -match("CLUSTER", colnames(model.df))]
  reg.model[[i]] <- lm(AMT_CREDIT ~., data = model.df)            # Create model for each cluster
  fit.accr[[i]] <- accuracy(reg.model[[i]]$fitted.values, model.df$AMT_CREDIT)   # Save accuracy results
  model.df <- reg.valid.df[reg.valid.df$CLUSTER == as.character(i),]
  model.df <- model.df[, -match(c("CLUSTER", "AMT_CREDIT"), colnames(model.df))]
  linear.pred[[i]] <- predict(reg.model[[i]], model.df)        # Predict for each cluster
}
model.df <- reg.train.df[, -match("CLUSTER", colnames(reg.train.df))]
reg.model[["ALL"]] <- lm(AMT_CREDIT ~., data = model.df)     # Overall model
fit.accr[["ALL"]] <- accuracy(reg.model[["ALL"]]$fitted.values, model.df$AMT_CREDIT)
model.df <- reg.valid.df[, -match(c("CLUSTER", "AMT_CREDIT"), colnames(reg.valid.df))]
linear.pred[["ALL"]] <- predict(reg.model[["ALL"]], model.df)

# Fit of different clusters vs overall dataset
fit.accr[[1]]      # Cluster 1
fit.accr[[2]]      # Cluster 2
fit.accr[[3]]      # Cluster 3
fit.accr[[4]]      # Cluster 4
fit.accr[[5]]      # Cluster 5
fit.accr[["ALL"]]  # Overall training data


## Regression tree
rtree.pred <- list()
for (i in 1:clusters) {
  model.df <- reg.train.df[reg.train.df$CLUSTER == as.character(i),]
  model.df <- model.df[, -match("CLUSTER", colnames(model.df))]
  reg.model[[i]] <- rpart(AMT_CREDIT ~ ., data = model.df, cp = 0.000001, minsplit = 2)  # Create model for each cluster
  fit.accr[[i]] <- accuracy(predict(reg.model[[i]], model.df), model.df$AMT_CREDIT)    # Save accuracy results
  model.df <- reg.valid.df[reg.valid.df$CLUSTER == as.character(i),]
  model.df <- model.df[, -match(c("CLUSTER", "AMT_CREDIT"), colnames(model.df))]
  rtree.pred[[i]] <- predict(reg.model[[i]], model.df)          # Predict for each cluster
}
model.df <- reg.train.df[, -match("CLUSTER", colnames(reg.train.df))]
reg.model[["ALL"]] <- rpart(AMT_CREDIT ~ ., data = model.df, cp = 0.000001, minsplit = 2)     # Overall model
fit.accr[["ALL"]] <- accuracy(predict(reg.model[[i]], model.df), model.df$AMT_CREDIT)
model.df <- reg.valid.df[, -match(c("CLUSTER", "AMT_CREDIT"), colnames(reg.valid.df))]
rtree.pred[["ALL"]] <- predict(reg.model[["ALL"]], model.df) 

# Fit of different clusters vs overall dataset
fit.accr[[1]]      # Cluster 1
fit.accr[[2]]      # Cluster 2
fit.accr[[3]]      # Cluster 3
fit.accr[[4]]      # Cluster 4
fit.accr[[5]]      # Cluster 5
fit.accr[["ALL"]]  # Overall training data

##############################################################################################################################

### Predicting TARGET (to give loan or not)

# Initialization
class.model <- list()
reg.model <- list()
set.seed(5)
train.rows <- sample(rownames(hc.df), dim(hc.df)[1]*0.6)
valid.rows <- setdiff(row.names(hc.df), train.rows)
class.train.df <- cbind(hc.df[train.rows, predictors[["NUMERICAL"]]], CLUSTER = hc.df[train.rows, "CLUSTER"],
                        hc.dummy[train.rows,])
class.valid.df <- cbind(hc.df[valid.rows, predictors[["NUMERICAL"]]], CLUSTER = hc.df[valid.rows, "CLUSTER"],
                        hc.dummy[valid.rows,])

## Logistic regression
logistic.pred <- list()
for (i in 1:clusters) {
  model.df <- class.train.df[class.train.df$CLUSTER == as.character(i),]
  model.df <- model.df[, -match("CLUSTER", colnames(model.df))]
  class.model[[i]] <- glm(TARGET ~., data = model.df, family = "binomial")   # Create model for each cluster
  fit.accr[[i]] <- confusionMatrix(as.factor(ifelse(class.model[[i]]$fitted.values > 0.5, 1, 0)),
                                   as.factor(model.df$TARGET))    # Confusion matrix for training data
  model.df <- class.valid.df[class.valid.df$CLUSTER == as.character(i),]
  model.df <- model.df[, -match(c("CLUSTER", "TARGET"), colnames(model.df))]
  logistic.pred[[i]] <- predict(class.model[[i]], model.df, type = "response")  # Predict for each cluster
}
model.df <- class.train.df[, -match("CLUSTER", colnames(class.train.df))]
class.model[["ALL"]] <- glm(TARGET ~., data = model.df, family = "binomial")     # Overall model
fit.accr[["ALL"]] <- confusionMatrix(as.factor(ifelse(class.model[["ALL"]]$fitted.values > 0.5, 1, 0)),
                                     as.factor(model.df$TARGET))
model.df <- class.valid.df[, -match(c("CLUSTER", "TARGET"), colnames(class.valid.df))]
logistic.pred[["ALL"]] <- predict(class.model[["ALL"]], model.df, type = "response")

# Fit of different clusters vs overall dataset
fit.accr[[1]]      # Cluster 1
fit.accr[[2]]      # Cluster 2
fit.accr[[3]]      # Cluster 3
fit.accr[[4]]      # Cluster 4
fit.accr[[5]]      # Cluster 5
fit.accr[["ALL"]]  # Overall training data


## Classification tree
ctree.pred <- list()
for (i in 1:clusters) {
  model.df <- class.train.df[class.train.df$CLUSTER == as.character(i),]
  model.df <- model.df[, -match("CLUSTER", colnames(model.df))]
  class.model[[i]] <- rpart(TARGET ~ ., data = model.df, method = "class", cp = 0.000001, minsplit = 2) # Create model for each cluster
  fit.accr[[i]] <- confusionMatrix(as.factor(predict(class.model[[i]], model.df,type="class")),
                                   as.factor(model.df$TARGET))      # Confusion matrix for training data
  model.df <- class.valid.df[class.valid.df$CLUSTER == as.character(i),]
  model.df <- model.df[, -match(c("CLUSTER", "TARGET"), colnames(model.df))]
  ctree.pred[[i]] <- predict(class.model[[i]], model.df, type = "class") # Predict for each cluster
}
model.df <- class.train.df[, -match("CLUSTER", colnames(class.train.df))]
class.model[["ALL"]] <- rpart(TARGET ~ ., data = model.df, method = "class", cp = 0.000001, minsplit = 2) # Overall model
fit.accr[["ALL"]] <- confusionMatrix(as.factor(predict(class.model[[i]], model.df,type="class")),
                                     as.factor(model.df$TARGET))
model.df <- class.valid.df[, -match(c("CLUSTER", "TARGET"), colnames(class.valid.df))]
ctree.pred[["ALL"]] <- predict(class.model[["ALL"]], model.df, type = "class")

# Fit of different clusters vs overall dataset
fit.accr[[1]]      # Cluster 1
fit.accr[[2]]      # Cluster 2
fit.accr[[3]]      # Cluster 3
fit.accr[[4]]      # Cluster 4
fit.accr[[5]]      # Cluster 5
fit.accr[["ALL"]]  # Overall training data



########################
#                      #
#   DSM Evaluation     #
#                      #
########################

### Evaluating predictions for AMT_CREDIT
## Linear regression

# Accuracy test
accuracy <- list()
for (i in 1:clusters) { 
  model.df <- reg.valid.df[reg.valid.df$CLUSTER == as.character(i), "AMT_CREDIT"]
  accuracy[[i]] <- accuracy(linear.pred[[i]], model.df)
}
accuracy[["ALL"]] <- accuracy(linear.pred[["ALL"]], reg.valid.df$AMT_CREDIT)

accuracy[[1]]      # Cluster 1
accuracy[[2]]      # Cluster 2
accuracy[[3]]      # Cluster 3
accuracy[[4]]      # Cluster 4
accuracy[[5]]      # Cluster 5
accuracy[["ALL"]]  # Overall 


### Residuals
residuals <- list()
for (i in 1:clusters) { 
  residuals[[i]] <- linear.pred[[i]] - reg.valid.df[reg.valid.df$CLUSTER == as.character(i), "AMT_CREDIT"]
}
residuals[["ALL"]] <- linear.pred[["ALL"]] - reg.valid.df$AMT_CREDIT

# Boxplots
options(scipen = 999)
ggplot(data.frame(Error = c(residuals[[1]],
                            residuals[[2]],
                            residuals[[3]],
                            residuals[[4]],
                            residuals[[5]],
                            residuals[["ALL"]]), 
                  Cluster = c(rep("1", dim(reg.valid.df[reg.valid.df$CLUSTER == "1",])[1]),
                              rep("2", dim(reg.valid.df[reg.valid.df$CLUSTER == "2",])[1]),
                              rep("3", dim(reg.valid.df[reg.valid.df$CLUSTER == "3",])[1]),
                              rep("4", dim(reg.valid.df[reg.valid.df$CLUSTER == "4",])[1]),
                              rep("5", dim(reg.valid.df[reg.valid.df$CLUSTER == "5",])[1]),
                              rep("All", dim(reg.valid.df)[1]))), 
       aes(x = Cluster, y = Error)) + 
  geom_boxplot(outlier.color = "red",
               outlier.shape = 24,
               outlier.fill = "red",
               outlier.size = 3,
               na.rm = TRUE) +
  xlab("Cluster") + ylab("Error") + ggtitle("Box plots for residuals")

# Mean
data.frame(Mean = c(mean(residuals[[1]]),
                    mean(residuals[[2]]),
                    mean(residuals[[3]]),
                    mean(residuals[[4]]),
                    mean(residuals[[5]]),
                    mean(residuals[["ALL"]])))


### Regression tree

# Accuracy test
accuracy <- list()
for (i in 1:clusters) { 
  model.df <- reg.valid.df[reg.valid.df$CLUSTER == as.character(i), "AMT_CREDIT"]
  accuracy[[i]] <- accuracy(rtree.pred[[i]], model.df)
}
accuracy[["ALL"]] <- accuracy(rtree.pred[["ALL"]], reg.valid.df$AMT_CREDIT)

accuracy[[1]]      # Cluster 1
accuracy[[2]]      # Cluster 2
accuracy[[3]]      # Cluster 3
accuracy[[4]]      # Cluster 4
accuracy[[5]]      # Cluster 5
accuracy[["ALL"]]  # Overall 



##############################################################################################################################

### Evaluating predictions for TARGET
## Logistic regression

# Confusion matrices based on cutoffs of 0.5, 0.25, and 0.75
conf.matrix.25 <- list()
conf.matrix.50 <- list()
conf.matrix.75 <- list()
for (i in 1:clusters) {
  model.df <- class.valid.df[class.valid.df$CLUSTER == as.character(i), "TARGET"]
  conf.matrix.25[[i]] <- confusionMatrix(as.factor(ifelse(logistic.pred[[i]] > 0.25, 1, 0)),as.factor(model.df))
  conf.matrix.50[[i]] <- confusionMatrix(as.factor(ifelse(logistic.pred[[i]] > 0.5, 1, 0)),as.factor(model.df))
  conf.matrix.75[[i]] <- confusionMatrix(as.factor(ifelse(logistic.pred[[i]] > 0.75, 1, 0)),as.factor(model.df))
}
conf.matrix.25[["ALL"]] <- confusionMatrix(as.factor(ifelse(logistic.pred[["ALL"]] > 0.25, 1, 0)),
                                           as.factor(class.valid.df[, "TARGET"]))
conf.matrix.50[["ALL"]] <- confusionMatrix(as.factor(ifelse(logistic.pred[["ALL"]] > 0.5, 1, 0)),
                                           as.factor(class.valid.df[, "TARGET"]))
conf.matrix.75[["ALL"]] <- confusionMatrix(as.factor(ifelse(logistic.pred[["ALL"]] > 0.75, 1, 0)),
                                           as.factor(class.valid.df[, "TARGET"]))

# Confusion matrices with cut-off of 0.25
conf.matrix.25[[1]]      # Cluster 1
conf.matrix.25[[2]]      # Cluster 2
conf.matrix.25[[3]]      # Cluster 3
conf.matrix.25[[4]]      # Cluster 4
conf.matrix.25[[5]]      # Cluster 5
conf.matrix.25[["ALL"]]  # Overall 

# Confusion matrices with cut-off of 0.5
conf.matrix.50[[1]]      # Cluster 1
conf.matrix.50[[2]]      # Cluster 2
conf.matrix.50[[3]]      # Cluster 3
conf.matrix.50[[4]]      # Cluster 4
conf.matrix.50[[5]]      # Cluster 5
conf.matrix.50[["ALL"]]  # Overall

# Confusion matrices with cut-off of 0.75
conf.matrix.75[[1]]      # Cluster 1
conf.matrix.75[[2]]      # Cluster 2
conf.matrix.75[[3]]      # Cluster 3
conf.matrix.75[[4]]      # Cluster 4
conf.matrix.75[[5]]      # Cluster 5
conf.matrix.75[["ALL"]]  # Overall


# New cutoff generated assuming cost of false negative(giving a loan) = 1.5*cost of false positive(not giving a loan)
pred <- list()
perf <- list()
cutoff <- list()
for (i in 1:clusters) {
  model.df <- class.valid.df[class.valid.df$CLUSTER == as.character(i), "TARGET"]
  pred[[i]] <- prediction(predictions = logistic.pred[[i]],labels = model.df)
  perf[[i]] <- performance(pred[[i]], "tpr", "fpr")
  cutoff[[i]] <- performance(pred[[i]], "cost", cost.fn = 1.5)
  cutoff[[i]] <- pred[[i]]@cutoffs[[1]][which.min(cutoff[[i]]@y.values[[1]])]
}
pred[["ALL"]] <- prediction(predictions = logistic.pred[["ALL"]],labels = class.valid.df$TARGET)
perf[["ALL"]] <- performance(pred[["ALL"]], "tpr", "fpr")
cutoff[["ALL"]] <- performance(pred[["ALL"]], "cost", cost.fn = 1.5)
cutoff[["ALL"]] <- pred[["ALL"]]@cutoffs[[1]][which.min(cutoff[["ALL"]]@y.values[[1]])]

# Cutoff values
cutoff[[1]]      # Cluster 1
cutoff[[2]]      # Cluster 2
cutoff[[3]]      # Cluster 3
cutoff[[4]]      # Cluster 4
cutoff[[5]]      # Cluster 5
cutoff[["ALL"]]  # Overall 


# Confusion matrix for new cutoffs
conf.matrix <- list()
for (i in 1:clusters) {
  model.df <- class.valid.df[class.valid.df$CLUSTER == as.character(i), "TARGET"]
  conf.matrix[[i]] <- confusionMatrix(as.factor(ifelse(logistic.pred[[i]] > cutoff[[i]], 1, 0)),as.factor(model.df))
}
conf.matrix[["ALL"]] <- confusionMatrix(as.factor(ifelse(logistic.pred[["ALL"]] > cutoff[["ALL"]], 1, 0)),
                                           as.factor(class.valid.df[, "TARGET"]))

conf.matrix[[1]]      # Cluster 1
conf.matrix[[2]]      # Cluster 2
conf.matrix[[3]]      # Cluster 3
conf.matrix[[4]]      # Cluster 4
conf.matrix[[5]]      # Cluster 5
conf.matrix[["ALL"]]  # Overall 


## Classification tree

# Confusion matrix
conf.matrix <- list()
for (i in 1:clusters) {
  model.df <- class.valid.df[class.valid.df$CLUSTER == as.character(i), "TARGET"]
  conf.matrix[[i]] <- confusionMatrix(as.factor(ctree.pred[[i]]),as.factor(model.df))
}
conf.matrix[["ALL"]] <- confusionMatrix(as.factor(ctree.pred[["ALL"]]), as.factor(class.valid.df[, "TARGET"]))

conf.matrix[[1]]      # Cluster 1
conf.matrix[[2]]      # Cluster 2
conf.matrix[[3]]      # Cluster 3
conf.matrix[[4]]      # Cluster 4
conf.matrix[[5]]      # Cluster 5
conf.matrix[["ALL"]]  # Overall 

## Decile chart discussions
gain <- gains(reg.valid.df[reg.valid.df$CLUSTER == "1", "AMT_CREDIT"], rtree.pred[[1]])
barplot(gain$mean.resp / mean(reg.valid.df[reg.valid.df$CLUSTER == "1", "AMT_CREDIT"]),
        names.arg = gain$depth, xlab = "Percentile", 
        ylab = "Mean Response", main = "Decile-wise lift chart")
