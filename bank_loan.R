
###-----Bank Marketing Analysis-----###

# Read data
library(readr)
data <- read_csv("D:/programs/R/bank_marketing.csv")

View(data)

# Structure of data
str(data)

#Summary of data_variables
summary(data)

###----- Understanding business requirements-----###
# Target variable is response
summary(data$response)

# Response variable is in character so, we need find factors
data$response <- as.factor(data$response)
levels(data$response)
table(data$response)

# The data is imbalanced data set, so we are finding Response Rate
responseRate <- 4640/(36548+4640) 
responseRate

# Finding missing value if any, in given data
sum(is.na(data))

###-----Data Understanding - Clint Information-----###
###-----Data Cleaning - Data Preparation-----###
###----- Age

#ggplot library for graphs
library(ggplot2)

#histogram plot for variable age
ggplot(data, aes(age))+geom_histogram()

# Let's check Outliers
quantile(data$age, seq(0, 1, 0.01))

# Boxplot
boxplot(data$age)

#Capping the variable age to overcome outliers
data[(which(data$age>62)), ]$age <- 62

boxplot(data$age)

# Check skewness of variable age
#library(moments)
#skewness(data$age)

# Binning the variable to save in binning.age
data$binning.age <- 
  as.factor(cut(data$age, breaks = c(16, 25, 34, 43, 52, 62)))
View(data)

ggplot(data, aes(age))+geom_histogram()

# Converting response variable as integer type for better processing
# yes/no to 1/0
data$response <- ifelse(data$response == "yes", 1, 0)

# Response rate in each bin
ageAggregate <- merge(aggregate(response ~ binning.age, data, mean),
                      aggregate(response ~ binning.age, data, sum), 
                      by="binning.age")
ageAggregate

# Adding Total no of Count to aggregate table
count <- data.frame(table(data$binning.age))
count

count <- count[, -1]
count

# Binding two table values
ageAggregate <- cbind(ageAggregate, count)
ageAggregate

# Giving Column names
colnames(ageAggregate) <- c("age", "responseRate", "response_yes", 
                            "TotalCount")
ageAggregate

# Round value
ageAggregate$responseRate <- format(round(ageAggregate$responseRate, 2))
ageAggregate

# plot to see the values of Response Rate of each bin of age
ggplot(ageAggregate, aes(age, TotalCount)) + geom_bar(stat="identity", 
      color = "red") + geom_text(size=5,
      label=ageAggregate$responseRate, nudge_x = 0.55, color = "green")
#------------------------------------------------------------------#

#----- Job
str(data$job)
data$job <- as.factor(data$job)
table(data$job)

# Unknown(less) values are combined with value admin.(large no values)
levels(data$job)[12] <- "admin."
table(data$job)

### ----- Creating plot_response Function -----###
# Calculates aggregate values of category variable to response variable
# Calculates response Rate in each category of category variable
# Create aggregate table 
# Plot to see response Rate of each category in variable
plot_response <- function(cat_var, var_name){
  
  a <- aggregate(response ~ cat_var, data, mean)
  
  count <- data.frame(table(cat_var))
  count <- count[, -1]
  
  agg <- cbind(a, count)
  colnames(agg) <- c(var_name, "responseRate", "count")
  
  agg$responseRate <- format(round(agg$responseRate, 2))
  View(agg)
  
  ggplot(agg, aes(agg[, 1], count))+geom_bar(stat ="identity",
          color="red")+geom_text(label=agg$responseRate, color="blue", 
          size=5, angle = 60)
}

plot_response(data$job, "job")
#---------------------------------------------------------------------#

#----- Marital
str(data$marital)
data$marital <- as.factor(data$marital)
table(data$marital)

levels(data$marital)[4] <- "married"
table(data$marital)

plot_response(data$marital, "marital")
#---------------------------------------------------------------------#

#----- Education
str(data$education)
data$education <- as.factor(data$education)
table(data$education)

levels(data$education)[5] <- "basic.4y"
table(data$education)

levels(data$education)[1:3] <- "primary"
table(data$education)

plot_response(data$education, "education")
#---------------------------------------------------------------------#

#----- Default
str(data$default)
data$default <- as.factor(data$default)
table(data$default)

# It is complete imbalanced variable in data it may cause loss 
# in model efficiency it is better to remove this variable 
data <- data[, -5]
View(data)
#---------------------------------------------------------------------#

#----- Housing
str(data$housing)
data$housing <- as.factor(data$housing)
table(data$housing)

levels(data$housing)[2] <- "yes"
table(data$housing)

plot_response(data$housing, "housing")
#---------------------------------------------------------------------#

#----- Loan
str(data$loan)
data$loan <- as.factor(data$loan)
table(data$loan)

levels(data$loan)[2] <- "no"
table(data$loan)

plot_response(data$loan, "loan")
#---------------------------------------------------------------------#

#----- Contact
plot_response(data$contact, "contact")
#---------------------------------------------------------------------#

#----- Month
plot_response(data$month, "month")
#---------------------------------------------------------------------#

#----- Day_of_week
plot_response(data$day_of_week, "day_of_week")
#---------------------------------------------------------------------#

#----- Duration
summary(data$duration)
ggplot(data, aes(duration))+geom_histogram()
boxplot(data$duration)
quantile(data$duration, seq(0, 1, 0.01))

agg <- aggregate(duration ~ response, data, mean)
agg
# can not understand any in-sights from duration variables
# stop it here
#---------------------------------------------------------------------#

#----- Campaign
summary(data$campaign)
ggplot(data, aes(campaign))+geom_histogram()
quantile(data$campaign, seq(0, 1, 0.01))
boxplot(data$campaign)
data[which(data$campaign > 11),]$campaign <- 11
#---------------------------------------------------------------------#

#----- Pdays
summary(data$pdays)
data$pdays <- as.factor(data$pdays)
levels(data$pdays)
table(data$pdays)

# Found 999 is nothing but 0
levels(data$pdays)[27] <- "0"

levels(data$pdays)[1:10] <- "before_10_days"
levels(data$pdays)

levels(data$pdays)[2:7] <- "after_10_days"
levels(data$pdays)

levels(data$pdays)[3:12] <- "first_time"
levels(data$pdays)

plot_response(data$pdays, "pdays")
#---------------------------------------------------------------------#

#----- Previous
summary(data$previous)
data$previous <- as.factor(data$previous)
table(data$previous)
levels(data$previous)

levels(data$previous)[1] <- "never_called"
levels(data$previous)

levels(data$previous)[2:4] <- "3_times_called"
levels(data$previous)

levels(data$previous)[3:6] <- "morethan_3_times"
levels(data$previous)

plot_response(data$previous, "previous")
#---------------------------------------------------------------------#

#----- Poutcome
summary(data$poutcome)
data$poutcome <- as.factor(data$poutcome)
table(data$poutcome)

plot_response(data$poutcome, "poutcomes")
#---------------------------------------------------------------------#

###----- Govt data -----###
# put it as it is until you know or discussed its process 
# about its corresponding mangers

#----- emp.var.rate
summary(data$emp.var.rate)
ggplot(data, aes(emp.var.rate))+geom_histogram()
skewness(data$emp.var.rate)

#----- cons.price.idx
summary(data$cons.price.idx)
ggplot(data, aes(cons.price.idx))+geom_histogram()
skewness(data$cons.price.idx)

#----- cons.conf.idx
summary(data$cons.conf.idx)
ggplot(data, aes(cons.conf.idx))+geom_histogram()
skewness(data$cons.conf.idx)

#----- euribor3m
summary(data$euribor3m)
ggplot(data, aes(euribor3m))+geom_histogram()
skewness(data$euribor3m)

#----- nr.employed
summary(data$nr.employed)
ggplot(data, aes(nr.employed))+geom_histogram()
skewness(data$nr.employed)
###----- All variables are processed and ready for Modeling
#--------------------------------------------------------------------#

#-------------------------------------------------------------------#
#----- Adding Column name as Customer_id
id <- rownames(data)
data <- cbind(id, data)
colnames(data)[1] <- "customer_id"
View(data)

data1 <- data
data2 <- data

#----- Creating Dummy variables for categorical variables
library(dummies)
data$response <- as.integer(data$response)
View(data)

data <- cbind(data[ ,1], dummy.data.frame(data[ , -1]))
colnames(data)[1] <- "C_id"

data$response <- as.factor(ifelse(data$response==1, "yes", "no"))
summary(data$response)

#----- Split Data into train and test data
library(caTools)
set.seed(1)
split <- sample.split(data$response, SplitRatio=0.70)
train <- data[split, ]
View(train)

test <- data[!split, ]
View(test)

nrow(train)/nrow(data)
nrow(test)/nrow(data)

# removing duration variable from train and test
train <- train[, -43]
View(train)

test <- test[, -43]
View(test)

###----- Model Building -----###
#----- Logistic Regression -----#
logistic <- glm(response ~., family = "binomial", data=train[, -1])
summary(logistic)

###----- stepAIC for finding best fit variable for model 
library(MASS)
stepwise <- stepAIC(logistic, direction = "both")
summary(stepwise)

#----- stepAIC gives best fit variables for the corresponding Algorithm
logistic2 <- glm(formula = response ~ jobadmin. + `jobblue-collar` + 
                   jobmanagement + jobretired + `jobself-employed` + 
                   jobservices + educationprimary + 
                   educationuniversity.degree + contactcellular + 
                   monthapr + monthjul + monthjun + monthmar + monthmay + 
                   monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                   campaign + previousnever_called + poutcomefailure + 
                   emp.var.rate + cons.price.idx + cons.conf.idx + 
                   nr.employed +`binning.age(16,25]`+`binning.age(25,34]`, 
                    family = "binomial", data = train[, -1])
summary(logistic2)

#----- vif for finding correlation between variables in the model
library(car)
vif(logistic2)

# Removing "emp.var.rate" variable.
logistic3 <- glm(formula = response ~ jobadmin. + `jobblue-collar` + 
                   jobmanagement + jobretired + `jobself-employed` + 
                   jobservices + educationprimary + 
                   educationuniversity.degree + contactcellular + 
                   monthapr + monthjul + monthjun + monthmar + monthmay + 
                   monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                   campaign + previousnever_called + poutcomefailure + 
                  cons.price.idx + cons.conf.idx + 
                   nr.employed +`binning.age(16,25]`+`binning.age(25,34]`, 
                 family = "binomial", data = train[, -1])
summary(logistic3)
vif(logistic3)

# Removing "monthmay" variable.
logistic4 <- glm(formula = response ~ jobadmin. + `jobblue-collar` + 
                   jobmanagement + jobretired + `jobself-employed` + 
                   jobservices + educationprimary + 
                   educationuniversity.degree + contactcellular + 
                   monthapr + monthjul + monthjun + monthmar + 
                   monthnov + monthoct + day_of_weekfri + day_of_weekmon + 
                   campaign + previousnever_called + poutcomefailure + 
                   cons.price.idx + cons.conf.idx + 
                   nr.employed +`binning.age(16,25]`+`binning.age(25,34]`, 
                 family = "binomial", data = train[, -1])
vif(logistic4)
summary(logistic4)

# Removing all unimportant variables from the model 
logistic5 <- glm(formula = response ~ `jobblue-collar` + 
                  jobretired + jobservices +  contactcellular + 
                   monthapr + monthjul + monthjun + monthmar + 
                   monthoct + day_of_weekmon + campaign + 
                   previousnever_called + poutcomefailure +cons.conf.idx +
                   nr.employed +`binning.age(16,25]`+`binning.age(25,34]`, 
                 family = "binomial", data = train[, -1])
vif(logistic5)
summary(logistic5)

#----- predicting model for test data
prediction <- predict(logistic5, newdata = test[ , -c(1, 58)], 
                      type = "response")
summary(prediction)
str(prediction)

###----- Model Evaluation -----###
#----- cut off 0.5
pred_response <- factor(ifelse(prediction >= 0.50, "yes", "no"))
pred_response

#----- ConfusionMatrix for show the efficiency of generated model
library(caret)
conf <- confusionMatrix(pred_response, test$response, positive="yes")
conf

result <- cbind(test$response, pred_response)
View(result)

#----- Save the model for further usage
save(logistic5, file = "/model.rda")

#--------------------------------------------------------------------#
