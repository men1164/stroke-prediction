install.packages("tidybayes")
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)

# Description
Data <- read.csv("healthcare-dataset-stroke-data.csv", sep = ",")
Data_summary <- summary(Data)
Data_head <- head(Data)

# Preparation
unique(Data$gender)
unique(Data$ever_married)
unique(Data$work_type)
unique(Data$Residence_type)
unique(Data$smoking_status)

# Checkink NA Values
colSums(is.na(Data))

# remove Gender: 'Other' 
# Change Categorical Variables -> Factors
stroke_mod <- Data %>%
  drop_na() %>%
  filter(gender != "Other", bmi != "N/A") %>%
  mutate(hypertension = factor(ifelse(hypertension == 1, "yes", "no")),
         heart_disease = factor(ifelse(heart_disease == 1, "yes", "no")),
         stroke = factor(ifelse(stroke == 1, "yes", "no")),
         bmi = as.double(bmi)) %>%
  mutate_if(is.character, as.factor)

# Visualization (Slide 18-21)
# Find Probability for each variable. (Modified for each)
Data_Prop <- stroke_mod %>%
  group_by(Residence_type) %>%
  summarise(prop = sum(stroke == "yes")/length(Residence_type))

df1 <- Data_Prop %>%
  ggplot(aes(x = gender, 
             y = prop)) +
  geom_col(fill = "#D8662A")

df2 <- Data_Prop %>%
  ggplot(aes(x = hypertension, 
             y = prop)) +
  geom_col(fill = "#D8662A")

df3 <- Data_Prop %>%
  ggplot(aes(x = heart_disease, 
             y = prop)) +
  geom_col(fill = "#D8662A")

df4 <- Data_Prop %>%
  ggplot(aes(x = ever_married, 
             y = prop)) +
  geom_col(fill = "#D8662A")

df5 <- Data_Prop %>%
  ggplot(aes(x = work_type, 
             y = prop)) +
  geom_col(fill = "#D8662A")

df6 <- Data_Prop %>%
  ggplot(aes(x = Residence_type, 
             y = prop)) +
  geom_col(fill = "#D8662A")

#------------------------------------------------------
# Let’s consider Female Versus Male (Slide 22)

# Male
stroke_mod %>%
  filter(gender == 'Male') -> stroke_male

# Female
stroke_mod %>%
  filter(gender == 'Female') -> stroke_female

# Ploting (Slide 23-24)
#--------------------- Age ----------------------------
ggplot(stroke_male, aes(x = gender,
                        y = age,
                        fill = stroke)) +
  geom_boxplot()

ggplot(stroke_female, aes(x = gender,
                          y = age,
                          fill = stroke)) +
  geom_boxplot()

# Ploting (Slide 25-26)
#--------------------- Level Glucose ------------------
ggplot(stroke_male, aes(x = gender,
                        y = avg_glucose_level,
                        fill = stroke)) +
  geom_boxplot()

ggplot(stroke_female, aes(x = gender,
                          y = avg_glucose_level,
                          fill = stroke)) +
  geom_boxplot()

# Ploting (Slide 27-28)
#--------------------- Hypertension ------------------
ggplot(stroke_male, aes(x = hypertension,
                        fill = stroke)) +
  geom_bar()

ggplot(stroke_female, aes(x = hypertension,
                          fill = stroke)) +
  geom_bar()

# In term of Probability (do both male and female)
Data_Prop2 <- stroke_male %>%
  group_by(hypertension) %>%
  summarise(prop = sum(stroke == "yes")/length(hypertension))

Data_Prop2 %>%
  ggplot(aes(x = hypertension, 
             y = prop)) +
  geom_col(fill = "#6DB04A")
# Do again for female

# Ploting (Slide 29-30)
#--------------------- Heart Disease ------------------
ggplot(stroke_male, aes(x = heart_disease,
                        fill = stroke)) +
  geom_bar()

ggplot(stroke_female, aes(x = heart_disease,
                          fill = stroke)) +
  geom_bar()

# In term of Probability (do both male and female)
Data_Prop3 <- stroke_male %>%
  group_by(heart_disease) %>%
  summarise(prop = sum(stroke == "yes")/length(heart_disease))

Data_Prop3 %>%
  ggplot(aes(x = heart_disease,
             y = prop)) +
  geom_col(fill = "#6DB04A")
# Do again for female
