
#Load necessary package
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggridges)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(fmsb)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(vcd)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(xgboost)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(class)) install.packages("caret", repos = "http://cran.us.r-project.org")




library(tidyverse)
library(caret)

#Importing the data
original_data <- read_csv("ADULT_DATASET.csv")

#Inspecting the data
str(original_data)

length(original_data)

#Data cleaning
sum(is.na(original_data))

#removing the ? from the data set.
clean_dataset <- filter(original_data, 
                       !workclass == "?", 
                       !occupation == "?", 
                       !native.country == "?")

# Convert character columns to factors using a for loop
for(col in names(clean_dataset)) {
  if(is.character(clean_dataset[[col]])) {
    clean_dataset[[col]] <- factor(clean_dataset[[col]])
  }
}
str(clean_dataset)

#Now let's save the cleaned data !
saveRDS(clean_dataset,file="clean_dataset.rds")
#-------------------thank

clean_dataset <- readRDS("clean_dataset.rds")


#splitting the data
set.seed(123)
test_index <- createDataPartition(y = clean_dataset$income, times = 1, p = 0.2, list = FALSE)
trainning_set <- clean_dataset[-test_index,]
validation_set <- clean_dataset[test_index,]


#Data exploration
summary(trainning_set)

#Data visualization

#histogram of age
ggplot(trainning_set, aes(x=age)) + geom_histogram(binwidth=5, fill="blue", color="black") +
  labs(title="Distribution of Ages", x="Age", y="Frequency")


#Bar Plot of Work Class
ggplot(trainning_set, aes(x=workclass)) + geom_bar(fill="green") + 
  labs(title="Count by Work Class", x="Work Class", y="Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Boxplot of Hours per Week by Income
ggplot(trainning_set, aes(x=income, y=hours.per.week)) + geom_boxplot() +
  labs(title="Hours per Week by Income Level", x="Income", y="Hours per Week")

#Bar Plot of Education Levels
ggplot(trainning_set, aes(x=education)) + geom_bar(fill="orange") + 
  labs(title="Count by Education Level", x="Education Level", y="Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Density Plot of Ages by Income
ggplot(trainning_set, aes(x=age, fill=income)) + geom_density(alpha=0.5) +
  labs(title="Age Distribution by Income", x="Age", y="Density")

#Count of Individuals by Marital Status and Income
ggplot(trainning_set, aes(x=marital.status, fill=income)) +
  geom_bar(position="dodge") +
  labs(title="Count by Marital Status & Income", x="Marital Status", y="Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis text for better readability
        legend.title = element_blank())  # Remove the legend title


#Facet Grid: To visualize the distribution of a variable across multiple facets.
ggplot(trainning_set, aes(x=age)) +
  geom_histogram(binwidth=5, fill="blue", alpha=0.7) +
  labs(title="Distribution of Age by Income and Sex", x="Age", y="Count") +
  facet_grid(sex ~ income)

#ScatterPlot To see the relationship between two numeric variables. E.g., age vs. hours.per.week
ggplot(trainning_set, aes(x=age, y=hours.per.week)) +
  geom_point(alpha=0.5) +
  labs(title="Scatter Plot of Age vs. Hours per Week", x="Age", y="Hours per Week")

#Workclass Distribution by Income
ggplot(trainning_set, aes(x=workclass, fill=income)) + 
  geom_bar(position="dodge") +
  labs(title="Count by Workclass & Income", x="Workclass", y="Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.title = element_blank())

#Hours per Week vs Age colored by Income
ggplot(trainning_set, aes(x=age, y=hours.per.week, color=income)) + 
  geom_point(alpha=0.5) +
  labs(title="Hours per Week vs Age", x="Age", y="Hours per Week") +
  theme_minimal()

#Education Level Counts
ggplot(trainning_set, aes(x=education, fill=income)) +
  geom_bar(position="dodge") +
  labs(title="Count by Education Level & Income", x="Education Level", y="Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.title = element_blank())

#Income Distribution by Gender
ggplot(trainning_set, aes(x=sex, fill=income)) + 
  geom_bar(position="fill") +
  labs(title="Income Distribution by Gender", x="Gender", y="Proportion") +
  theme_minimal()

#Occupation Distribution by Income
ggplot(trainning_set, aes(x=occupation, fill=income)) + 
  geom_bar(position="dodge") +
  labs(title="Count by Occupation & Income", x="Occupation", y="Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.title = element_blank())


ggplot(trainning_set, aes(x=age, fill=income)) + 
  geom_histogram(binwidth=5, position="identity", alpha=0.5) +
  labs(title="Age Distribution by Income", x="Age", y="Count") +
  theme_minimal()

#Correlation Heatmap
library(corrplot)
cor_matrix <- cor(trainning_set[sapply(trainning_set, is.numeric)])
corrplot(cor_matrix, method="circle")


#Ridgeline Plot of Age Distribution by Education
library(ggridges)
ggplot(trainning_set, aes(x = age, y = education, fill = income)) + 
  geom_density_ridges() +
  labs(title="Ridgeline Plot of Age Distribution by Education Level", x="Age") +
  theme_ridges()

library(plotly)
trainning_set %>%
  count(workclass) %>%
  plot_ly(labels = ~workclass, values = ~n, type = "pie") %>%
  layout(title = "Workclass Distribution")

#Boxplot of Hours per Week by Occupation
ggplot(trainning_set, aes(x=occupation, y=hours.per.week, fill=occupation)) + 
  geom_boxplot() +
  labs(title="Hours per Week by Occupation", x="Occupation", y="Hours per Week") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none")

#Facet by Education
ggplot(trainning_set, aes(x=hours.per.week, fill=income)) + 
  geom_histogram(binwidth=2, position="identity", alpha=0.5) +
  facet_wrap(~education, scales="free") +
  labs(title="Hours per Week Distribution by Education and Income", x="Hours per Week", y="Count") +
  theme_minimal()

#Table of Mean Hours per Week by Occupation
library(knitr)
occupation_table <- aggregate(trainning_set$hours.per.week ~ trainning_set$occupation, trainning_set, mean)
names(occupation_table) <- c("Occupation", "Mean Hours per Week")
kable(occupation_table, format="markdown", align="l", col.names=c("Occupation", "Mean Hours per Week"))


#Radar/Spider Chart for Numeric Data
library(fmsb)
data_spider <- trainning_set[, sapply(trainning_set, is.numeric)]
data_spider$income <- trainning_set$income
data_spider <- aggregate(.~income, data_spider, mean)
data_spider <- rbind(max(data_spider[,-1]), data_spider)
colnames(data_spider) <- c("income", names(data_spider)[-1])
radarchart(data_spider)


library(vcd)
mosaicplot(table(trainning_set$relationship, trainning_set$income), main="Mosaic Plot of Relationship Status vs. Income", shade=TRUE)

#Machine learning model -----------------------------
#model 1 : logistic regression
model_logistic <- glm(income ~ ., family=binomial(link='logit'), data=trainning_set)
pred_logistic <- predict(model_logistic, newdata=validation_set, type="response")
result_logistic <- ifelse(pred_logistic > 0.5, ">50K", "<=50K")
confusionMatrix(factor(result_logistic), validation_set$income)

#model 2 : random forest
library(randomForest)
set.seed(123)
model_rf <- randomForest(income ~ ., data=trainning_set, ntree=100)
pred_rf <- predict(model_rf, newdata=validation_set)
confusionMatrix(pred_rf, validation_set$income)

#model 3 : XGBOOST
library(xgboost)
dtrain <- xgb.DMatrix(data = model.matrix(~.-1, data=trainning_set[,!names(trainning_set) %in% c('income')]), label = as.numeric(trainning_set$income == ">50K"))
dtest <- xgb.DMatrix(data = model.matrix(~.-1, data=validation_set[,!names(validation_set) %in% c('income')]), label = as.numeric(validation_set$income == ">50K"))

params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, depth=6)
set.seed(123)
model_xgb <- xgb.train(params = params, data = dtrain, nrounds=100)
pred_xgb <- predict(model_xgb, newdata=dtest)
result_xgb <- ifelse(pred_xgb > 0.5, ">50K", "<=50K")
confusionMatrix(factor(result_xgb), validation_set$income)


#Model 4 : Support vector machine
library(e1071)
model_svm <- svm(income ~ ., data=trainning_set, type='C-classification', kernel='radial')
pred_svm <- predict(model_svm, newdata=validation_set)
confusionMatrix(pred_svm, validation_set$income)


#Model 5 : K-Nearest Neighbors
library(class)
train_data_matrix <- as.matrix(model.matrix(income ~ ., trainning_set)[, -1])
val_data_matrix <- as.matrix(model.matrix(income ~ ., validation_set)[, -1])
pred_knn <- knn(train_data_matrix, val_data_matrix, cl = trainning_set$income, k = 21)
confusionMatrix(pred_knn, validation_set$income)



#TABLE OF EACH MODEL------------------------------------
# Ensure the income variable in the validation set is a factor with specified levels
validation_set$income <- factor(validation_set$income, levels = c("<=50K", ">50K"))

# Convert the predictions to factors with the same levels as validation_set$income
predictions$logistic <- factor(predictions$logistic, levels = c("<=50K", ">50K"))
predictions$rf <- factor(predictions$rf, levels = c("<=50K", ">50K"))
predictions$xgboost <- factor(predictions$xgboost, levels = c("<=50K", ">50K"))
predictions$svm <- factor(predictions$svm, levels = c("<=50K", ">50K"))
predictions$knn <- factor(predictions$knn, levels = c("<=50K", ">50K"))

# Initialize an empty dataframe to store results
model_performance <- data.frame(
  Model = character(),
  Accuracy = numeric(),
  F1_Score = numeric(),
  stringsAsFactors = FALSE
)

# Compute accuracy and F1 scores based on the predictions
for(model_name in names(predictions)) {
  cm <- confusionMatrix(predictions[[model_name]], validation_set$income)
  
  accuracy <- sum(diag(cm$table)) / sum(cm$table)
  
  recall <- cm$byClass['Recall']
  precision <- cm$byClass['Precision']
  
  f1 <- 2 * (precision * recall) / (precision + recall)
  
  model_performance <- rbind(model_performance, data.frame(Model = model_name, Accuracy = accuracy, F1_Score = f1))
}

model_performance


