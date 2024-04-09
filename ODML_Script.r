#-----1. Data Reading-------------------------------------------

# set working directory
setwd(dirname(file.choose()))
getwd()

# read in data from csv file
Occ_data <- read.csv(file.choose())
head(Occ_data)
str(Occ_data)


#-----2. Data Exploration-------------------------------------------
attach(Occ_data)

# check for missing data
apply(Occ_data, MARGIN = 2, FUN = function(x) sum(is.na(x)))
library(Amelia)
missmap(Occ_data, y.labels =dates, col = c("black", "pink"), legend = TRUE)

# remove duplicates
Occ_data <- unique(Occ_data)

# select relevant variables
Occ_data2 <- Occ_data[2:7]

# Checking if data is normally distributed
detach(Occ_data)
attach(Occ_data2)
# summary of variables
summary(Occ_data2[1:5])

boxplot(Occ_data2[1:2], xlab="Variables", ylab="Count", col = "pink")
# boxplot for variables
boxplot(Occ_data2[3:4], xlab="Variables", ylab="Count", col = "pink")
boxplot(Occ_data2[5], xlab="Humidy ratio", ylab="Count", col = "pink")

# create a violin plot of Sepal.Length by Species
library(vioplot)

vioplot(Occ_data2[1:2], col = "pink", xlab = "Variables", ylab = "Count")
vioplot(Occ_data2[3:4], col = "pink", xlab = "Variables", ylab = "Count")
vioplot(Occ_data2[5], col = "pink", xlab = "Variable", ylab = "Count")

#histogram to confirm normalization
#histogram for humidity
hist(humidity, col = "pink", border = "dark green", freq = F,
     xlab = "humidity", main = "Histogram")
rug (humidity)
lines (density(sort(humidity)))
xfit <- seq(from = min(humidity), to = max(humidity), by = 0.1)
yfit = dnorm(xfit, mean(humidity), sd(humidity))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)

#histogram for light
hist(light, col = "pink", border = "dark green", freq = F,
     xlab = "light", main = "Histogram")
rug (light)
lines (density(sort(light)))
xfit <- seq(from = min(light), to = max(light), by = 0.1)
yfit = dnorm(xfit, mean(light), sd(light))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)

#histogram for co_2
hist(co_2, col = "pink", border = "dark green", freq = F,
     xlab = "co_2", main = "Histogram")
rug (co_2)
lines (density(sort(co_2)))
xfit <- seq(from = min(co_2), to = max(co_2), by = 0.1)
yfit = dnorm(xfit, mean(co_2), sd(co_2))
lines(xfit, yfit, lty = "dotted")
rm(xfit, yfit)
legend("topright", legend = c("Density curve", "Normal curve"),
       lty = c("solid", "dotted"), cex = 0.7)


# correlation of all the variables
# correlation matrix
corr_test <- cor(Occ_data2, method = "pearson")
corr_test <- round(corr_test, digits = 2)

# correlation plot
install.packages("rlang")
update.packages("rlang")
library(ggplot2)
library(GGally)
GGally::ggpairs(Occ_data2)
ggpairs(Occ_data2, columns = 1:6,
                ggplot2::aes(colour="blue"),
                upper = list(continuous = "cor"),
                title = "Correlation plot")


library(corrplot)
corrplot(corr_test)

# qqplot for variables
qqnorm(temperature, xlab = "Theoretical Quantiles: Temperature", col="pink" )
qqline(temperature, col="black")

qqnorm(humidity, xlab = "Theoretical Quantiles: Humidity", col="pink")
qqline(humidity, col="black")

qqnorm(humidityratio, xlab = "Theoretical Quantiles: Humidity Ratio", col="pink")
qqline(humidityratio, col="black")

qqnorm(occupancy, xlab = "Theoretical Quantiles: Occupancy", col="pink")
qqline(occupancy, col="black")

# KINDLY IGNORE....WAS JUST PLAYING AROUND
#install.packages("rgl")
#library(rgl)
#plot3d(temperature, humidity, humidityratio, type="s", col="blue", size=2)
#title3d("3D Scatterplot Example")

#-----3. BINOMIAL LOGISTIC REGRESSION-------------------------------------------
# make Occupancy a factor
Occ_data2$occupancy <- factor(Occ_data2$occupancy)
str(Occ_data2)

# confirm data is balanced (For imbanced, take not of Accuracy & Sensitivity)
table(Occ_data2$occupancy)

# data partitioning
set.seed(246)
part <- sample(2, nrow(Occ_data), replace = T, prob = c(0.7, 0.3))

train_data <- Occ_data2[part==1,]
test_data <- Occ_data2[part==2,]

# model training
log_model1 = glm(occupancy ~., data = train_data, family = "binomial")
summary(log_model1)

# Calculate Odds Ratio - Exp(b) with 95% confidence intervals (2 tail)
exp(cbind(OR = coef(log_model1), confint(log_model1)))

#Evaluate the goodness of fit
library(pscl)
pR2(log_model1)

# reiduals check
plot(log_model1$residuals)

# model test
library(caret)
log_pred1 <- predict(log_model1, newdata = test_data, type = "response")
log_pred1 <- factor(ifelse(log_pred1 > 0.5, 1, 0), levels = c(1, 0))
log_pred1
conf_matrix1 <- confusionMatrix(log_pred1, test_data$occupancy, positive = "1")
conf_matrix1

log_recall1 <- conf_matrix1$byClass["Recall"]
log_f1_1 <- conf_matrix1$byClass["F1"]


# test for multicollinearity
library(car)
vif(log_model1)
sqrt(vif(log_model1)) > 2 


# LOGISTIC MODEL 2
log_model2 = glm(occupancy ~ temperature + humidity + light + co_2, data = train_data, family = "binomial")
summary(log_model2)

# Calculate Odds Ratio - Exp(b) with 95% confidence intervals (2 tail)
exp(cbind(OR = coef(log_model2), confint(log_model2)))

#Evaluate the goodness of fit
pR2(log_model2)


plot(log_model2$residuals)

log_pred2 <- predict(log_model2, newdata = test_data, type = "response")
log_pred2 <- factor(ifelse(log_pred2 > 0.5, 1, 0), levels = c(1, 0))
log_pred2
conf_matrix2 <- confusionMatrix(log_pred2, test_data$occupancy, positive = "1")
conf_matrix2

recall2 <- conf_matri2$byClass["Recall"]
f1_2 <- conf_matrix2$byClass["F1"]

vif(log_model2)
sqrt(vif(log_model2)) > 2 

# LOGISTIC MODEL 3 with OVERSAMPLING
table(train_data$occupancy)
over_train_data = upSample(train_data, train_data$occupancy)
table(over_train_data$occupancy)

log_model3 = glm(occupancy ~ temperature + humidity + light + co_2, data = over_train_data, family = "binomial")
summary(log_model3)

vif(log_model3)
sqrt(vif(log_model3)) > 2 

plot(log_model3$residuals)

log_pred3 <- predict(log_model3, newdata = test_data, type = "response")
log_pred3 <- factor(ifelse(log_pred3 > 0.5, 1, 0), levels = c(1, 0))
log_pred3
conf_matrix3 <- confusionMatrix(log_pred3, test_data$occupancy, positive = "1")
conf_matrix3

recall3 <- conf_matrix3$byClass["Recall"]
f1_3 <- conf_matrix3$byClass["F1"]


#-----4. DECISION TREE-------------------------------------------
install.packages("party")
library(party)

d_tree = ctree(occupancy~., train_data)
plot(d_tree)

dt_pred1 = predict(d_tree, test_data)
dt_pred1

# using rpart
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

d_tree2 = rpart(occupancy ~., data = train_data)
d_tree2 = rpart(occupancy ~ temperature + co_2 + humidity + humidityratio , data = train_data)
rpart.plot(d_tree2, main = "Decision Tree")
printcp(d_tree2)
dt_pred2 = predict(d_tree2, test_data)
dt_pred1


#-----5. RANDOM FOREST-------------------------------------------
install.packages("randomForest")
library(randomForest)

rf_model1 = randomForest(occupancy~., train_data)
plot(rf_model1)
print(rf_model1)

rf_pred1 = predict(rf_model1, test_data)
rf_pred1

conf_matrix6 <- confusionMatrix(rf_pred1, test_data$occupancy, positive = "1")
conf_matrix6

#-----6. K NEAREST NEIGHBOUR-------------------------------------------
install.packages("class")
library(class)

k_values <- data.frame(k = seq(1, 20, by = 2))

attach(Occ_data2)
Occ_data3 <- scale(cbind(temperature, humidity, light, co_2, humidityratio))
Occ_data3 <- merge(Occ_data3, Occ_data2[6])   
rm(Occ_data3)

train_data2 <- train_data
test_data2 <- test_data

train_data2$temperature <- scale(train_data2$temperature)
test_data2$temperature <- scale(test_data2$temperature)
train_data2$humidity <- scale(train_data2$humidity)
test_data2$humidity <- scale(test_data2$humidity)
train_data2$light <- scale(train_data2$light)
test_data2$light <- scale(test_data2$light)
train_data2$co_2 <- scale(train_data2$co_2)
test_data2$co_2 <- scale(test_data2$co_2)
train_data2$humidityratio <- scale(train_data2$humidityratio)
test_data2$humidityratio <- scale(test_data2$humidityratio)

# Create a KNN model using cross-validation and grid search
knn_model1 <- train(occupancy ~ ., data = train_data2, method = "knn", 
                   trControl = trainControl(method = "cv", number = 10),
                   tuneGrid = k_values)
knn_model1$results

# Plot the accuracy as a function of K
ggplot(data = knn_model1$results, aes(x = k, y = Accuracy)) +
  geom_line() +
  geom_point() +
  labs(x = "K", y = "Accuracy") +
  ggtitle("KNN Model 1")

# Predict the test set using the trained model
knn_pred1 <- predict(knn_model1, test_data2)
knn_pred1

conf_matrix4 <- confusionMatrix(knn_pred1, test_data2$occupancy, positive = "1")
conf_matrix4

knn_f1_1 <- conf_matrix4$byClass["F1"]


# Create a KNN model using cross-validation and grid search
knn_model2 <- train(occupancy ~ temperature + humidity + light + co_2, data = train_data2, method = "knn", 
                    trControl = trainControl(method = "cv", number = 10),
                    tuneGrid = k_values)
knn_model2$results

# Plot the accuracy as a function of K
ggplot(data = knn_model2$results, aes(x = k, y = Accuracy)) +
  geom_line() +
  geom_point() +
  labs(x = "K", y = "Accuracy") +
  ggtitle("KNN Model 2")

# Predict the test set using the trained model
knn_pred2 <- predict(knn_model1, test_data2)
knn_pred2

conf_matrix5 <- confusionMatrix(knn_pred2, test_data2$occupancy, positive = "1")
conf_matrix5

knn_f1_2 <- conf_matrix5$byClass["F1"]

#-----7. NAIVE BAYES-------------------------------------------
install.packages("naivebayes")
install.packages("e1071")
library(naivebayes)
library(caret)
library(e1071)

# Train the Naive Bayes model
nb_model1 <- train(occupancy ~ ., data = train_data, method = "naive_bayes", trControl = trainControl(method = "cv", number = 10), na.action = na.pass, tuneLength = 10)
nb_model1

# Predict the test set using the trained model
nb_pred1 <- predict(nb_model1, test_data)
nb_pred1

# Confusion matrix
conf_matrix7 <- confusionMatrix(nb_pred1, test_data$occupancy, positive = "1")
conf_matrix7

nb_f1_1 <- conf_matrix7$byClass["F1"]




#not part of project-----------------------
install.packages(c("bigrquery", "DBI"))

library(bigrquery)
library(DBI)

Sys.setenv(KAGGLE_CONFIG_PATH = "C:/Users/damil/OneDrive/Documents/STUDY/Kaggle.json")
# Replace path/to/your/key.json with the actual path to your JSON key file.
bq_auth(path = "C:/Users/damil/OneDrive/Documents/STUDY/Kaggle.json")

project_id <- "bigquery-public-data"
dataset_id <- "london_crime"
datasets <- bq_dataset_tables(project_id)
print(datasets)

query <- paste0("SELECT * FROM `", project_id, ".", dataset_id, ".crime_by_lsoa`")
results <- bq_table_download(bq_project_query(project = project_id, query = query))

# Create a BigQuery connection
con <- dbConnect(
  bigrquery::bigquery(),
  project = project_id,
)

# Execute the query and retrieve the results
results <- dbGetQuery(con, query)

# Close the connection
dbDisconnect(con)

# Print the data
print(results)


# # loading sentiment word lists
# positive = scan('opinion-lexicon-English (2)/opinion-lexicon-English/positive-words.txt', what = 'character', comment.char = ';')
# negative = scan('opinion-lexicon-English (2)/opinion-lexicon-English/negative-words.txt', what = 'character', comment.char = ';')
# # add your list of words below as you wish if missing in above read lists
# pos.words = c(positive,'upgrade','Congrats','prizes','prize','thanks','thnx',
#               'Grt','gr8','plz','trending','recovering','brainstorm','leader',
#               'order', 'prevention', 'safety', 'brave', 'justice', 'hero', 'inspiring',
#               'support', 'courage', 'unity', 'resilience', 'solidarity', 'empathy', 'hope')
# neg.words = c(negative,'wtf','wait','waiting','epicfail','Fight','fighting',
#               'arrest','no','not', 'shocking', 'tragic', 'disturbing', 'outrage',
#               'anguish', 'heartbreaking', 'injustice', 'unfair', 'frustration', 'anger')
# 
# # sentiment scoring function
# score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
# {
#   require(plyr)
#   require(stringr)
#   
#   # we are giving vector of sentences as input. 
#   # plyr will handle a list or a vector as an "l" for us
#   # we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
#   scores = laply(sentences, function(sentence, pos.words, neg.words) {
#     
#     # clean up sentences with R's regex-driven global substitute, gsub() function:
#     sentence = gsub('https://','',sentence)
#     sentence = gsub('http://','',sentence)
#     sentence = gsub('[^[:graph:]]', ' ',sentence)
#     sentence = gsub('[[:punct:]]', '', sentence)
#     sentence = gsub('[[:cntrl:]]', '', sentence)
#     sentence = gsub('\\d+', '', sentence)
#     sentence = str_replace_all(sentence,"[^[:graph:]]", " ")
#     # and convert to lower case:
#     sentence = tolower(sentence)
#     
#     # split into words. str_split is in the stringr package
#     word.list = str_split(sentence, '\\s+')
#     # sometimes a list() is one level of hierarchy too much
#     words = unlist(word.list)
#     
#     # compare our words to the dictionaries of positive & negative terms
#     pos.matches = match(words, pos.words)
#     neg.matches = match(words, neg.words)
#     
#     # match() returns the position of the matched term or NA
#     # we just want a TRUE/FALSE:
#     pos.matches = !is.na(pos.matches)
#     neg.matches = !is.na(neg.matches)
#     
#     # TRUE/FALSE will be treated as 1/0 by sum():
#     score = sum(pos.matches) - sum(neg.matches)
#     
#     return(score)
#   }, pos.words, neg.words, .progress=.progress )
#   
#   scores.df = data.frame(score=scores, text=sentences)
#   return(scores.df)
# }
# 
# # calculating the sentiment score
# analysis <- score.sentiment(cleanText, pos.words, neg.words)
# # sentiment score frequency table
# table(analysis$score)