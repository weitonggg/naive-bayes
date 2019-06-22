# fitting naivebayes model 
library(dplyr)
library(naivebayes)
library(pROC)
# load data
heart <- read.csv("heart.csv")
colnames(heart) <- c("age", "sex", "chest_pain", "bp_rest",
                     "cholesterol", "bloodsugar_high", "ecardio_rest",
                     "max_heartrate", "exer_angina", "oldpeak", "slope",
                     "major_ves", "thal", "target")
summary(heart)
# arrange by feature class
heart <- heart %>% select(age, bp_rest, cholesterol,
                          max_heartrate, oldpeak, major_ves, 
                          sex, chest_pain, bloodsugar_high, ecardio_rest,
                          exer_angina, slope, thal, target)
# set categorical features to class factor 
for (i in 7:14){
  heart[,i] <- as.factor(heart[,i])
}

# separate into training-validate-test: 70-30
set.seed(100)
indices <- sample(1:2, size = nrow(heart), prob = c(0.7, 0.3), replace = TRUE)
training <- heart[indices == 1, ]
testing <- heart[indices == 2, ]

nb_model <- naive_bayes(target~ ., 
                        data = training, laplace = 1)
nb_class <- predict(nb_model, testing[,-14], type="class")
nb_prob <- predict(nb_model, testing[,-14], type = "prob")

# confusion matrix
table(nb_class, testing[,14])
# accuracy
sum(nb_class == testing[,14]) # around 87%
# plot ROC 
plot.roc(testing[,14], nb_prob[,1], col ="blue", lwd = 3, print.auc = TRUE)
