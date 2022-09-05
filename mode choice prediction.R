setwd("C:/Users/kelly/OneDrive/Documents/Stats with R Certificate/Quarter 3/Homework/Final Project")
load("Project_prep.RData")

#decision tree for trip mode choice (mode_simple)

library(randomForest)
library(dplyr)
library(stats)

#split train/test data
set.seed("1234")

choice_data = choice_data[complete.cases(choice_data),] #remove NAs so that all vectors are same length.
ndata = nrow(choice_data)
ratio = 0.7
data_train = sample(1:ndata, ndata * 0.7)
train_data = choice_data[data_train,] 
test_data = choice_data[-data_train,]


table(train_data$mode_simple)

library(party)
tree = ctree(mode_simple ~ dest_purpose_cat + age + gender + education + o_puma10 + hhincome_broad + seattle_home +
       transit_benefits + final_home_puma10 + trip_path_distance + license + travelers_total +
       vehicle_count, data = train_data)

plot(tree)


random_forest = randomForest(mode_simple ~ dest_purpose_cat + age + gender + education + o_puma10 + hhincome_broad + seattle_home +
                               transit_benefits + final_home_puma10 + trip_path_distance + license + travelers_total +
                               vehicle_count, data = train_data, na.action = na.pass) 

prediction = predict(random_forest, test_data)


#table of predictions
plot(random_forest)
tab2 = table(prediction)
mode_split_prediction = prop.table(tab2)
drive_pct = mode_split_prediction["Drive"]*100

#confusion matrix
random_forest$call
library(caret)
modeCM = confusionMatrix(as.factor(test_data$mode_simple), as.factor(prediction))
pander::pander(modeCM$table)
#total actuals will vary due to randomness, but I think set.seed accounts for this. 

#####
#try decision tree again but under sample drive data.

library(rpart)
#install.packages("ROSE")
library(ROSE)
choice_under <- ovun.sample(drive ~ ., data = choice_data, method = "under", p=0.5)$data 
table(choice_under$drive)

ndata = nrow(choice_under)
ratio = 0.7
data_train = sample(1:ndata, ndata * 0.7)
train_data = choice_under[data_train,] 
test_data = choice_under[-data_train,]


table(train_data$mode_simple)


random_forest = randomForest(mode_simple ~ dest_purpose_cat + age + gender + education + o_puma10 + hhincome_broad + seattle_home +
                               transit_benefits + final_home_puma10 + trip_path_distance + license + travelers_total +
                               vehicle_count, data = train_data, na.action = na.pass) 



prediction = predict(random_forest, test_data)


#table of predictions
plot(random_forest)
tab2 = table(prediction)
mode_split_prediction = prop.table(tab2)
drive_pct = mode_split_prediction["Drive"]*100

#confusion matrix
random_forest$call
library(caret)
modeCM = confusionMatrix(as.factor(test_data$mode_simple), as.factor(prediction))
pander::pander(modeCM$table)

sum(prediction)


