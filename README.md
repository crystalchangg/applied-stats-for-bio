# applied-stats-for-bio
sta100
link to project: https://docs.google.com/document/d/176EWzqu7hr3_hF4kdtniHyNafnwa_SuP93R3MSV2m34/edit?usp=sharing

dataset: [insurancesta100projectdataset.csv](https://github.com/user-attachments/files/22062011/insurancesta100projectdataset.csv)

```
title: "Health Insurance Charges"
author: "Crystal Chang"
date: "`r Sys.Date()`"
output:
  word_document: default
  html_document: default
  pdf_document: default
```

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```

```{r, echo = TRUE, eval = TRUE}
library(ggplot2)
library(ggpubr)
library(forcats)

# Import the Data
insurance = read.csv("~/Downloads/insurancesta100projectdataset.csv")

# Create Age vs Insurance Charge: Bar Graph
age1 = ggplot(insurance, aes(age, charges)) +
  geom_bar(stat = "summary", fun = "mean", aes(fill = age))+
  scale_x_continuous(breaks=seq(0,70,5))+
  scale_y_continuous(breaks=seq(0,40000,5000))+
  labs(title = "Age vs Average Insurance Charge")+
  labs(x = "Age")+ labs(y = "Average Insurance Charge ($)")+
  guides(fill = "none")+
  geom_smooth(method="lm", color = "red")+
  stat_regline_equation(label.x=20, label.y=17000)+
  theme_classic()

# Create Sex vs Insurance Charge: Boxplots
sex1 = ggplot(insurance, aes(sex, charges))+
  geom_boxplot()+
  labs(title = "Sex vs Insurance Charge")+
  scale_y_continuous(breaks=seq(0,70000,10000))+
  labs(x = "Sex")+ labs(y = "Insurance Charge ($)")+
  theme_classic()

# Create BMI vs Insurance Charge: Scatter Plot
bmi1 = ggplot(insurance, aes(bmi, charges))+
  labs(x = "BMI")+ labs(y = "Insurance Charge ($)")+
  geom_point()+
  scale_y_continuous(breaks=seq(0,70000,10000))+
  labs(title = "BMI vs Average Insurance Charge")+
  labs(x = "BMI")+ labs(y = "Average Insurance Charge ($)")+
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth", color = "red")+
  stat_regline_equation(label.x=20, label.y=50000)+
  theme_classic()

# Create Children vs Insurance Charge: Bar Graph
child = ggplot(insurance, aes(children, charges)) + 
  geom_bar(stat = "summary", fun = "mean", aes(fill = children))+
  labs(title = "Children vs Insurance Charge")+
  labs(x = "Number Of Children")+ labs(y = "Average Insurance Charge ($)")+
  scale_x_continuous(breaks=seq(0,5,1))+
  guides(fill = "none")+
  theme_classic()

# Create Regions vs Insurance Charge: Bar Graph 
region1 =  ggplot(insurance, aes(region, charges)) + 
  geom_bar(stat = "summary", fun = "mean", aes(fill = region))+
  coord_flip()+
  labs(title = "Region vs Insurance Charge")+
  labs(x = "Region in the U.S.")+ labs(y = "Average Insurance Charge ($)")+
  guides(fill = "none")+
  theme_classic()

# Create Smoker vs Insurance Charge: Boxplots 
smokers = ggplot(insurance, aes(x=fct_inorder(smoker), charges))+
  geom_boxplot()+
  labs(title = "Smoker vs Insurance Charge")+
  scale_y_continuous(breaks=seq(0,70000,10000))+
  labs(x = "Smoker (Yes/No)")+ labs(y = "Insurance Charge ($)")+
  theme_classic()

# This is to rearrange the graphs into columns
ggarrange(sex1, smokers, child, age1, ncol = 2)
region1
bmi1

# Making dummy variables out of the categorical variables
East = fct_collapse(insurance$region,
             East = c("northeast", "southeast"))
male = ifelse(insurance$sex == 'male', 1, 0)
female = ifelse(insurance$sex == 'female', 1, 0)
smoker = ifelse(insurance$smoker == 'yes', 1, 0)
region = ifelse(East == 'East', 1, 0)

# Create a data frame that includes all dummy variables
df_insurance = data.frame(charges = insurance$charges,
                     Age = insurance$age,
                     Sex = female,
                     Smoker = smoker,
                     Region = region,
                     Children = insurance$children,
                     BMI = insurance$bmi)

#This is the proposed multiple linear regression model
proposed_model = lm(charges ~ Age + Sex + BMI + Children + Smoker + Region, 
                    data = df_insurance)
summary(proposed_model)
#This is the fitted multiple linear regression model
fitted_model = lm(charges ~ Age + BMI + Children + Smoker, data = df_insurance)
summary(fitted_model)
```


