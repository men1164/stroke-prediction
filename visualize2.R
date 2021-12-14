library(tidyverse)
df <- read.csv("healthcare-dataset-stroke-data.csv")

df %>%
  group_by(gender) %>%
  filter(bmi != "N/A",gender != "Other") %>%
  mutate(hypertension = factor(ifelse(hypertension == 1, "yes", "no")),
         heart_disease = factor(ifelse(heart_disease == 1, "yes", "no")),
         stroke = factor(ifelse(stroke == 1, "yes", "no")),
         bmi = as.double(bmi)) %>% 
  mutate_if(is.character, as.factor) -> stroke_mod
  
  summary(stroke_mod)
  
#gender
  ggplot(stroke_mod,aes(gender,fill = stroke)) +
  geom_bar() +
    labs(title = "Gender:stroke")
  
#hypertension 
  ggplot(stroke_mod,aes(hypertension,fill = stroke))+
    geom_bar()+
    labs(title = "Hypertension:stroke")

#heart disease  
  ggplot(stroke_mod,aes(heart_disease,fill = stroke))+
    geom_bar()+
    labs(title = "Heart disease:stroke")
  
#married
  ggplot(stroke_mod,aes(ever_married,fill = stroke))+
    geom_bar()+
    labs(title = "Ever-married:stroke")
  
  
#work-type
  ggplot(stroke_mod,aes(work_type,fill = stroke))+
    geom_bar() +
      labs(title = "Hypertension:stroke")
  

#residence
  ggplot(stroke_mod,aes(Residence_type,fill = stroke))+
    geom_bar()+
      labs(title = "Residence:stroke")

#smoking status
  ggplot(stroke_mod,aes(smoking_status,fill = stroke))+
    geom_bar()+
      labs(title = "Smoking status:stroke")
  