##Case  Problem-Books By Mail from Paul Green by LDA
library(MASS)
library(caret)
library(biotools)
library(dplyr)
library(doBy)
library(ggplot2)
library(scales)
library(gtools)
###########
### Load data
#########
data_df <- read.table("C:/Jaya/GL/Predictive Modelling/GA/PaulBooks2.csv",sep = ",", header = T)
data_df[,4]<-as.factor(data_df[,4])

###########
### Data Examination
#########
head(data_df)
str(data_df)
summary(data_df)

### Data cleaning
### 1 Find if there are any missing values

cat("\n Variables with number of missing values \n")
sapply(data_df[,c(2:4)], function(x) sum(is.na(x))) # To report missing values

### Observation:There are **NO MISSING VALUES**

### 2 Find if there are any constant values else will give error for LDA

cat("\n Variables with constant values \n")
sapply(data_df[,c(2:4)], function(x) length(unique(x)))

### Observation: There are no constant values for the predictor variables.

### 3 Find if any variables have near zero variance.
#Remove columns with near zero variance as features should has variance in its distribution.

nzv <- nearZeroVar(data_df[,c(2:4)])
nzv

### Observation: There is no predictor variable with near zero variance.

library(psych)
describeBy(data_df[,c(2,3)], data_df$Purchase)


###########
### Visual Exploration
#########

##Correlation Plots
pairs.panels(data_df[2:4],
             gap = 0,
             bg = c("red","green")[data_df$Purchase],
             pch=21)

### Observation 

#1.  We find there is no linear relationship between pair of variables 
#2.  Correlation coefficient is also very low for these predictor variables - 0.11
#3. As Months since last purchase increases, the incidence of Purchase cases decreases.
#4. As no of art books purchased increases incidence of Purchase cases increases.


library(car)
scatterplotMatrix(data_df[2:4])
# From the scatterplot matrix we could see the linear relationship and the distribution between observed variables. 
# Months and NoBought at glance seemed to have non-normal distribution. 
# Relationship between Months-Purchase suggest have negative linear relationship.
# Relationship between NoBought-Purchase suggest have positive linear relationship.

library(ggplot2)
library(ggpubr)
p1 <- ggplot(data = data_df, aes(x = Months, fill = Purchase)) +
  geom_histogram(position = "identity", alpha = 0.5)
p2 <- ggplot(data = data_df, aes(x = NoBought, fill = Purchase)) +
  geom_histogram(position = "identity", alpha = 0.5)

ggarrange(p1, p2, nrow = 2, common.legend = TRUE)

## At the individual level, the No. of art books purchased seems to be the variable that most 
##differentiates between Purchase (less overlap between populations).

pairs(x = data_df[, c("Months","NoBought")],
      col = c("Red", "Green")[data_df$Purchase], pch = 18)

##3D scatter plot
library(scatterplot3d)
scatterplot3d(data_df$Months, data_df$NoBought,
              color = c("firebrick", "green3")[data_df$Purchase], pch = 19,
              grid = TRUE, xlab = "Months", ylab = "NoBought",
              angle = 65, cex.axis = 0.6)
legend("topleft",
       bty = "n", cex = 0.8,
       title = "Purchase",
       c("0", "1"), fill = c("firebrick", "green3"))

#########Plot for data without LDA
plotData <- ggplot(data = data_df,
                   mapping = aes(x = Months, y = NoBought, color = Purchase)) +
  geom_point(alpha = 0.5) +
  scale_color_manual(values = c("0" = "Red", "1" = "Green")) +
  theme_bw() +
  theme(legend.key = element_blank()) +
  labs(title = "Original data")
plotData

##BoxPlots

boxplot(data_df$Months ~ data_df$Purchase, col=c("Red","Green"),
        main="Pattern Revealed by Purchase",
        ylab="Months",xlab="Purchase" )
boxplot(data_df$NoBought ~ data_df$Purchase, col=c("Red","Green"),
        main="Pattern Revealed by Purchase",
        ylab="No. Bought",xlab="Purchase" )

# Same, but with different colors and add regression lines
ggplot(data_df, aes(x=Months, y=NoBought, color=Purchase)) +
  geom_point(shape=1) +
  scale_colour_hue(l=50) + # Use a slightly darker palette than normal
  geom_smooth(method=lm,   # Add linear regression lines
              se=FALSE)  + # Don't add shaded confidence region
  labs(title = "Purchase by Months & NoBought")
#### Interpretation

#For Purchase = 0

#Measure | Months | NoBought
#------- | -------| -------
#  Count | 919    | 919
#Average | 13.26  | 0.32

#For Purchase = 1

#Measure | Months | NoBought
#------- | -------| -------
#  Count | 81     | 81
#Average | 8.93   | 1.02

##### From the graph, we observe that data is linearly separable.



###########
### Assumption Test
#########
#Normality Test
#Linear Discriminant Analysis (LDA) require normality of the independent variables and 
#equal dispersion and covariance structures for the groups asn defined by the dependent 
#variables. In graphical test Months and NoBought was suggested have non-normal distribution. 
#Shapiro-Wilk test and Kolmogorv-Smirnov test employed to confirmed the previous presumption.
################################################################
#Distribution of predictors individually:
#Representation by Histogram of each variable for each Purchase
par(mfcol = c(2, 2))
for (k in 2:3) {
  j0 <- names(data_df)[k]
  #br0 <- seq(min(datos[, k]), max(datos[, k]), le = 11)
  x0 <- seq(min(data_df[, k]), max(data_df[, k]), le = 50)
  for (i in 1:2) {
    i0 <- levels(data_df$Purchase)[i]
    x <- data_df[data_df$Purchase == i0, j0]
    hist(x, proba = T, col = grey(0.8), main = paste("Purchase", i0),
         xlab = j0)
    lines(x0, dnorm(x0, mean(x), sd(x)), col = "red", lwd = 2)
  }
}

#Representation of normal quantiles of each variable for each Purchase
for (k in 2:3) {
  j0 <- names(data_df)[k]
  x0 <- seq(min(data_df[, k]), max(data_df[, k]), le = 50)
  for (i in 1:2) {
    i0 <- levels(data_df$Purchase)[i]
    x <- data_df[data_df$Purchase == i0, j0]
    qqnorm(x, main = paste("Purchase", i0, j0), pch = 21, col = i + 1)
    qqline(x)
  }
}

#Contrast of normality Shapiro-Wilk for each variable in each Purchase
library(reshape2)
library(knitr)
library(dplyr)
data_df_tidy <- melt(data_df[-1], value.name = "valor")
kable(data_df_tidy %>% group_by(Purchase, variable) %>% summarise(p_value_Shapiro.test = shapiro.test(valor)$p.value))
#The variables Months and NoBought are not distributed normally in the groups.

library(MVN)
hzTest(data = data_df[,2:3], qqplot = FALSE)
roystonTest(data = data_df[,2:3], qqplot = TRUE)
#######################################################################
library(MVN)
uniNorm(data_df[2:3], type = "SW", desc = TRUE) # Shapiro-Wilk Test
#The hypothesis of normality is rejected for all variables
#Group 0
s <- subset(data_df, data_df$Purchase == 0)
shapiro.test(t(as.matrix(s[,-4])))

#Group 1
s <- subset(data_df, data_df$Purchase == 1)
shapiro.test(t(as.matrix(s[,-4])))
#The hypothesis of normality is rejected for all groups

uniNorm(data_df[2:3], type = "Lillie", desc = FALSE) # Kolmogorov-Smirnov Test
##The hypothesis of normality is rejected for all variables
##Both tests show significant evidence of lack of multivariate normality. 
##The LDA has some robustness against the lack of multivariate normality, 
##but it is important to take it into account in the conclusion of the analysis

#Similarity of Dispersion (Homoscedasticy)
#Box's M test to assess the similarity of the dispersion matrices of the independent variables among group.
library(biotools)
hom<-boxM(data_df[,2:3],group = data_df$Purchase)
hom
#p???value in Box's M test is <0.01. Thus we reject the H0 hypothesis and conclusion is the 
#dispersion matrices of the independent variables among group is not equal. 
#If this is not fulfilled, we resort to Quadratic Discriminant Analysis ( QDA ).

#MANOVA
manova2 <- manova(as.matrix(data_df[,2:3])~data_df$Purchase, data = data_df)
summary(manova2, test = "Wilks")

#Discriminant analysis is significant
#All this is true for the normal model.
library(HDMD)
pooled_cov <- hom$pooled
mach <- pairwise.mahalanobis(data_df[,2:3],grouping = data_df$Purchase, cov = pooled_cov)
print(mach$distance)
#Hotelling test
library(Hotelling)
hotel<-hotelling.test(.~Purchase, data = data_df, pair = c(1,2))
hotel$pval
#p-value is 0, which means that both groups differ significantly

#Redundancy of traits
#Let's choose the attributes that affect the quality of the separation. 
#Forward Greedy Wilks (Stepwise Analysis).

PurchaseType <- as.numeric(data_df$Purchase)
greedy.wilks(data_df[,2:3], PurchaseType)
#Have received, that all signs render significant influence on result.

###########
### Discriminant Analysis
#########
### Split dataset into training dataset and testing dataset in the ratio 70 % : 30 %

df_train <- read.table("C:/Jaya/GL/Predictive Modelling/GA/PaulBooks2.csv",sep = ",", header = T)
df_train <- df_train[,c(2,3,4)]
df_train[,3]<-as.factor(df_train[,3])
str(df_train)

df_test <- read.table("C:/Jaya/GL/Predictive Modelling/GA/PaulBooks1.csv",sep = ",", header = T)
df_test <- df_test[,c(2,3,4)]
df_test[,3]<-as.factor(df_test[,3])
str(df_test)
#### Check the proportion of persons having Purchase values Y or N 

#### Check if distribution of partition data is correct Training dataset
prop.table((table(df_train$Purchase)))

#### Check if distribution of partition data is correct Testing dataset
prop.table((table(df_test$Purchase)))

### Observation

##Data partition is well balanced.**

## Performing LDA
library(MASS)
X=as.matrix(df_train[,1:2])
Y=as.vector(df_train[,3])

X1=as.matrix(df_test[,1:2])
Y1=as.vector(df_test[,3])

fit <- lda(df_train$Purchase ~ ., na.action="na.omit", data = df_train)

fit
plot(fit)
library(DiscriMiner)
x=df_train[,1:2]
y=df_train[,3]
Fisher=desDA(x,y)
Fisher

Mahalanobis = linDA(x,y)
Mahalanobis

Mahalanobis$scores

data3=data.frame(Mahalanobis$scores)
data3
attach(data3)
Prob0=exp(X0)/(exp(X0)+exp(X1))
Prob1=exp(X1)/(exp(X0)+exp(X1))
PosteriorProb=data.frame(Prob0,Prob1)
PosteriorProb
XtWO=cbind(as.matrix(df_train[,1:2]))
Manova=manova(XtWO~y)
Manova

summary(Manova)
summary(Manova, test="Wilks")
discPower(XtWO,y)

#Partition Plot
library(klaR)
partimat(Purchase~NoBought+Months,
         data=df_train,method="lda",prec = 200,
         #image.colors = c("Orange", "Blue"),
         col.mean = "Red")

####  The code below assesses the accuracy of the prediction.

#### check prediction on Training data

### Assess the accuracy of the prediction
### Percent correct for each category of Diabetesn

pred <- predict(fit,df_train)
CTable <- table(df_train[,3],pred$class) 

cat("\n\n --- Confusion Matrix ---\n\n")
print(CTable)
#      0   1
###0 894  25
###1  63  18

cat("\n OVerall Accuracy for training dataset \n")

### Total percent correct
sum(diag(prop.table(CTable)))
#0.912
trainig_error <- mean(data_df$Purchase != pred$class) * 100
paste("trainig_error =", trainig_error, "%")
#training error = 8.8%

#88 of the 1000 predictions that the model has made have been wrong. The trainig error 
#may said to be low (8.8%), which suggests that the model may be good. However, to validate it, 
#a new data set is necessary to calculate the error test or resort to cross-validation.

## Put into the dataset
df_train$predProbLda <- pred$posterior[,"1"]
df_train$predClassLda <- pred$class

## Plot (probability)
library(gridExtra)
plotLdaProb <- ggplot(data = df_train,
                      mapping = aes(x = Months, y = NoBought, color = predProbLda)) +
  geom_point(alpha = 0.5) +
  #layer(geom = "point", alpha = 0.5) +
  scale_color_gradient(low = "Red", high = "Green") +
  theme_bw() +
  theme(legend.key = element_blank()) +
  labs(title = "Predicted probability of outcome (LDA)")
grid.arrange(plotData, plotLdaProb, ncol = 2)

#### check prediction on Test data

pred1 <- predict(fit,df_test)

CTable1 <- table(df_test$Purchase,pred1$class)

print(CTable1)
#     0   1
###0 889  28
###1  63  20

cat("\n OVerall Accuracy for training dataset \n")

### Total percent correct
sum(diag(prop.table(CTable1)))
#0.909

Jacknife=lda(Y~X,CV=TRUE)
confusionJackknife=table(Original=df_train$Purchase,Predicted=Jacknife$class)
confusionJackknife
sum(diag(prop.table(confusionJackknife)))
#0.911

Jacknife.test=lda(Y1~X1,CV=TRUE)
confusionJackknife.test=table(Original=df_test$Purchase,Predicted=Jacknife.test$class)
confusionJackknife.test
sum(diag(prop.table(confusionJackknife.test)))
#.918

#### Evaluate model performance

##Measure | Training dataset Value | Testing dataset Value
##------- | ---------------------- | ----------------------
##Overall | 0.912                  | 0.909
##Accuracy|                        | 

##### Model performance is good for both training dataset and testing dataset


#Histogram
ldahist(data = pred$x, g = df_train$Purchase)
