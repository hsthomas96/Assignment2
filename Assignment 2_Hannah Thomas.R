#Assignment 2- Hannah Thomas

#Packages
install.packages("readr")
install.packages("table1")
install.packages("ggplot2")

#Libraries
library(readr)
library(table1)
library(ggplot2)

#Load dataset
CVrisk_data <- read_csv("raw-data/cohort.csv")
View(CVrisk_data)

#Descriptive table
#This is a dataset describing the patient-level (sex, age, smoking status, cardiac event) and hospital-level (cost of stay) characteristics of patients admitted to an inpatient cardiac ward.
#Variables and definitions:
#Female (renamed to sex)= biological sex status. Binary. Male= 0, Female= 1
#Age= age in years. Continuous.
#Smoke= history of smoking. Binary. Non-smoker=0, Smoker=1
#Cost= cost of inpatient admission in US dollars. Continuous.

CVrisk_data$cardiac <- factor(CVrisk_data$cardiac,
                              levels=c(0,1),
                              labels=c("No cardiac event",
                                      "Cardiac event"))

CVrisk_data$smoke <- factor(CVrisk_data$smoke,
                              levels=c(0,1),
                              labels=c("Non-smoker",
                                       "Smoker"))

CVrisk_data$female <- factor(CVrisk_data$female,
                              levels=c(0,1),
                              labels=c("Male",
                                       "Female"))

label(CVrisk_data$smoke) <-"Smoking status"
label(CVrisk_data$female) <- "Sex"
label(CVrisk_data$age) <- "Age (years)"
label(CVrisk_data$cost) <- "Cost (USD)"

caption <- "Descriptive Study Cohort"

table1(~female+age+smoke+cost | cardiac, data=CVrisk_data) #final descriptive table

#Regression model
#Null hypothesis: Costs associated with inpatient cardiac stay were different for female compared to male patients.
#Outcome= cost (USD)
#Predictors= female (ref group male), age, smoke (ref group non-smoker), presence/absence of cardiac event (ref group no cardiac event)

cost_model <- lm(cost~female+age+smoke+cardiac, data=CVrisk_data)
summary(cost_model)

#The model looks broadly correct as I would expect costs to rise with age, smoking status and cardiac event.

#Figure 1. Patient Age and Inpatient Costs, by Sex

ggplot(CVrisk_data, aes(x = age, y = cost, color=female)) +
  geom_point(alpha = 0.6) +  
  geom_smooth(method = "lm", se = TRUE) +  #Confidence interval
  labs(title = "Patient Age and Inpatient Costs, by Sex",
       x = "Age (years)",
       y = "Inpatient Cost (USD)",
       color= "Sex") +
  theme_minimal()

#I did not use generative AI technology to complete any portion of the work.
