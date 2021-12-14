library(tidyverse)
library(caret)

stroke_df <- read.csv("healthcare-dataset-stroke-data.csv")

# 1) Preparation
stroke_df %>% 
  drop_na() %>% 
  filter(gender != "Other", bmi != "N/A") %>% 
  mutate(hypertension = factor(ifelse(hypertension == 1, "yes", "no")),
         heart_disease = factor(ifelse(heart_disease == 1, "yes", "no")),
         stroke = factor(ifelse(stroke == 1, "yes", "no")),
         bmi = as.double(bmi)) %>% 
  mutate_if(is.character, as.factor) -> stroke_mod

summary(stroke_mod)

# 2) Separate by Gender
stroke_mod %>% 
  filter(gender == "Male") -> stroke_male


stroke_mod %>% 
  filter(gender == "Female") -> stroke_female


# 3) Modeling and Predict

# 3.1) Male
summary(stroke_male$stroke)
# By this, we can tell that the probability that male will get stroke is 0.044

# Lets separate the training and testing set. We will train by using 70% of all rows from stroke_male
set.seed(5000)
test_ind_male <- sample(nrow(stroke_male), 0.3*nrow(stroke_male))

stroke_male_train <- stroke_male[-test_ind_male,]
stroke_male_test <- stroke_male[test_ind_male,]

model_male <- glm(stroke ~ age * hypertension * heart_disease * avg_glucose_level, data = stroke_male_train, family = binomial)

res_male <- predict(model_male, stroke_male_test, type = "response")
summary(res_male)

# Factor that if male have a chance to get stroke more than 30% or not, depends on predictors
res_male_c <- factor(ifelse(res_male >= 0.3, "yes", "no"))
summary(res_male_c)


# 3.2) Female
summary(stroke_female$stroke)
# By this, we can tell that the probability that female will get stoke is 0.041

# Lets separate the training and testing set. We will train by using 70% of all rows from stroke_female
set.seed(5000)
test_ind_female <- sample(nrow(stroke_female), 0.3*nrow(stroke_female))

stroke_female_train <- stroke_female[-test_ind_female,]
stroke_female_test <- stroke_female[test_ind_female,]

model_female <- glm(stroke ~ age * hypertension * heart_disease * avg_glucose_level, data = stroke_female_train, family = binomial)

res_female <- predict(model_female, stroke_female_test, type = 'response')
summary(res_female)

# Factor that if female have a chance to get stroke more than 30% or not, depends on predictors
res_female_c <- factor(ifelse(res_female >= 0.3, "yes", "no"))
summary(res_female_c)


# 3.3) Overall
summary(stroke_mod$stroke)
#0.043

set.seed(20000)
test_ind <- sample(nrow(stroke_mod), 0.3*nrow(stroke_mod))

stroke_train <- stroke_mod[-test_ind,]
stroke_test <- stroke_mod[test_ind,]

model <- glm(stroke ~ age * hypertension * heart_disease * avg_glucose_level, data = stroke_train, family = binomial)

res <- predict(model, stroke_test, type = 'response')
summary(res)

res_c <- factor(ifelse(res >= 0.3, "yes", "no"))
summary(res_c)


# 4) Evaluation
# 4.1) Male
confusionMatrix(res_male_c, stroke_male_test$stroke, mode = 'prec_recall', positive = "yes")

lift_male <- data.frame(prob = res_male, stroke = stroke_male_test$stroke)

lift_obj_male <- lift(stroke ~ prob, data = lift_male, class = 'yes')
plot(lift_obj_male)


# 4.2) Female
confusionMatrix(res_female_c, stroke_female_test$stroke, mode = 'prec_recall', positive = "yes")

lift_female <- data.frame(prob = res_female, stroke = stroke_female_test$stroke)

lift_obj_female <- lift(stroke ~ prob, data = lift_female, class = 'yes')
plot(lift_obj_female)


# 4.3) Overall
confusionMatrix(res_c, stroke_test$stroke, mode = 'prec_recall', positive = 'yes')

lift_res <- data.frame(prob = res, stroke = stroke_test$stroke)

lift_obj <- lift(stroke ~ prob, data = lift_res, class = 'yes')
plot(lift_obj)



# ------------MODEL CV TRAIN [NOT USED]----------------
train_control <- trainControl(method = 'cv', number = 10)
model_2 <- train(stroke ~ age + hypertension + heart_disease + avg_glucose_level, data = stroke_female_train, trControl = train_control, method = 'glm', family = 'binomial')
model_2

res_2 <- predict(model_2, stroke_female_test, type = 'prob')

res_2_c <- factor(ifelse(res_2$yes >= 0.3, "yes", "no"))
summary(res_2_c)

confusionMatrix(res_2_c, stroke_female_test$stroke, positive = 'yes')
