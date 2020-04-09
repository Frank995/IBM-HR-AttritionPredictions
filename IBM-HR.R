###########################################################
# HarvardX PH125.9x Data Science Capstone
#
# IBM HR Analytics Employee Attrition & Performance
# Predict attrition of your valuable employees
#
# Francesco Pudda, 2020
###########################################################

################################
# Library import
################################

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(ggcorrplot)) install.packages("ggcorrplot", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(ggcorrplot)
library(caret)

# Import external function too
source("IBM-HR-DrawCM.R")

################################
# Data import and inspection
################################

# Import library
dataset <- read_csv("IBM-HR-Dataset.csv")

# Inspect first rows and check columns classes
head(dataset, 3)
table(sapply(dataset, class))

# Check for any NA
cat("Any NA?", any(is.na(dataset)), "\n")

# A bit of optimisation before starting the
# exploratory data analysis. Convert numeric
# to integer whenever possible, and character
# to factor.
dataset[,which(sapply(dataset, class) == "numeric")] <-
  lapply(dataset[,which(sapply(dataset, class) == "numeric")], as.integer)
dataset[,which(sapply(dataset, class) == "character")] <-
  lapply(dataset[,which(sapply(dataset, class) == "character")], as.factor)

# After this two operations dataset was converted to tibble which
# prevented some later operations.
dataset <- as.data.frame(dataset)

# Removing columns with the same value for every row or
# unused columns
dataset <- dataset %>%
  select(-EmployeeCount, -Over18, -StandardHours, -EmployeeNumber)

# Some feature are actually factors, even if listed as integers.
actualFactors <- c("Education", "EnvironmentSatisfaction", "JobInvolvement",
                   "JobSatisfaction", "PerformanceRating",
                   "RelationshipSatisfaction", "WorkLifeBalance")
for (i in 1:length(actualFactors)){
  dataset[, actualFactors[i]] <- as.factor(dataset[, actualFactors[i]])
}
levels(dataset$Education) <- c("Under college", "College", "Bachelor",
                               "Master", "Doctorate")
levels(dataset$EnvironmentSatisfaction) <- c("Low", "Medium",
                                             "High", "Very high")
levels(dataset$JobInvolvement) <- c("Low", "Medium",
                                    "High", "Very high")
levels(dataset$JobSatisfaction) <- c("Low", "Medium",
                                     "High", "Very high")
levels(dataset$PerformanceRating) <- c("Low", "Good",
                                       "Excellent", "Outstanding")
levels(dataset$RelationshipSatisfaction) <- c("Low", "Medium",
                                              "High", "Very high")
levels(dataset$WorkLifeBalance) <- c("Bad", "Good",
                                     "Better", "Best")

# Reorder factors for later visualisation
dataset$Attrition <- factor(dataset$Attrition, levels = c("Yes", "No"))

################################
# Exploratory data analysis
################################

# Let's check Attrition according to different
# variables to get an idea of the most important
# effects that may cause it.
# There will be a lot of graphs that will be discussed
# in the report. The code is pretty much similar
# one another, and when it isn't, it will be
# further commented.
# Since the dataset is made up by so many different
# columns considering them all, with all the inter
# relationships would be an overkill and in all
# likely not even necessary.

# Attrition percentage in general.
dataset %>%
  group_by(Attrition) %>%
  summarise(Average = n() / nrow(dataset)) %>%
  ggplot(aes(Attrition, Average, fill = Attrition)) +
  geom_col() +
  ggtitle("Attrition imbalance in dataset") +
  ylab("Percentage") +
  geom_text(aes(Attrition, Average + 0.04, label = round(Average, 2))) +
  scale_fill_manual(values=c("#FF0000", "#00FF00"))

## Gender factors ##

# Attrition percentage by gender.
dataset %>%
  group_by(Gender) %>%
  summarise(Attrition_perc = sum(Attrition == "Yes")/n()) %>%
  ggplot(aes(Gender, Attrition_perc, fill = Gender)) +
  geom_col() +
  ggtitle("Attrition percentage by gender") +
  ylab("Attrition percentage") +
  theme(legend.position = "none")

# Let's have a look at job satisfaction and
# position by gender.
# I'm using facet_Wrap to plot male and female
# next to each other and using a viridis palette
# to colour them.
dataset %>%
  select(JobLevel, JobSatisfaction, Gender) %>%
  mutate(JobSatisfaction = as.integer(JobSatisfaction)) %>%
  group_by(JobLevel, Gender) %>%
  summarise(Satisfaction = mean(JobSatisfaction)) %>%
  ggplot(aes(x = JobLevel, y = Satisfaction, group = JobLevel,
             fill = Satisfaction)) +
  geom_col() +
  facet_wrap(~Gender) +
  ggtitle("Average job satisfaction by gender and job level") +
  ylab("Average job satisfaction") +
  xlab("Job level")

# Check also differences in salary by gender.
dataset %>%
  group_by(Gender) %>%
  ggplot(aes(Gender, MonthlyIncome, fill = Gender)) +
  geom_boxplot() +
  ggtitle("Average salary by gender") +
  ylab("Salary") +
  theme(legend.position = "none")

## Age and generation factors ##

# Attrition percentage by age.
dataset %>%
  group_by(Age) %>%
  summarise(Attrition_perc = mean(Attrition == "Yes")) %>%
  ggplot(aes(Age, Attrition_perc)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Attrition percentage by age") +
  ylab("Attrition percentage")

# Define a function to return the generation
# name by the age assuming that this dataset
# was made in 2019 and considering that minimum
# age is 18.
# Generation Z: 1997-2010 (Age >= 9 & Age <= 22)
# Millenials:   1981-1996 (Age >= 23 & Age <= 38)
# Generation X: 1965-1980 (Age >= 39 & Age <= 54)
# Baby boomers: 1946-1964 (Age >= 55 & Age <= 73)
# I'm returning ad index so that when I convert them
# to factor they will be automatically sorted.
getGeneration <- function(age){
  if (age <= 22){
    return(4) #Generation Z
  }
  else if (age >= 23 & age <= 38){
    return(3) #Millenials
  }
  else if (age >= 39 & age <= 54){
    return(2) #Generation X
  }
  else{
    return(1) #Baby Boomers
  }
}

# Attrition by generation
dataset %>%
  mutate(Generation = factor(sapply(Age, getGeneration),
                             labels = c("Baby Boomers", "Generation X",
                                        "Millenials", "Generation Z"))) %>%
  group_by(Generation) %>%
  summarise(Attrition_perc = mean(Attrition == "Yes")) %>%
  ggplot(aes(Generation, Attrition_perc, fill = Generation)) +
  geom_col() +
  ggtitle("Attrition percentage by generation") +
  ylab("Attrition percentage") +
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none")

# Distribution of generations
dataset %>%
  mutate(Generation = factor(sapply(Age, getGeneration),
                             labels = c("Baby Boomers", "Generation X",
                                        "Millenials", "Generation Z"))) %>%
  group_by(Generation) %>%
  summarise(Proportion = n()/nrow(dataset)) %>%
  ggplot(aes(x = Generation, y = Proportion, fill = Generation)) +
  geom_col(position = "dodge") +
  ggtitle("Distribution of workers per generation") +
  ylab("Proportion") +
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none") +
  geom_text(aes(x = Generation, y = Proportion+0.03, label = round(Proportion,2)))

# Job level by generation
dataset %>%
  mutate(Generation = factor(sapply(Age, getGeneration),
                             labels = c("Baby Boomers", "Generation X",
                                        "Millenials", "Generation Z"))) %>%
  ggplot(aes(Generation, JobLevel, fill = Generation)) +
  geom_boxplot() +
  ggtitle("Job level by generation") +
  ylab("Job level") +
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.position = "none")

## Education and income factors ##

# Attrition percentage by monthly income.
# Income is rounded to the closest 200$ to have more
# data points to average.
dataset %>%
  mutate(MonthlyIncome = round(MonthlyIncome/200)*200) %>%
  group_by(MonthlyIncome) %>%
  summarise(Attrition_perc = mean(Attrition == "Yes")) %>%
  ggplot(aes(MonthlyIncome, Attrition_perc)) +
  geom_point() +
  geom_smooth() +
  ggtitle("Attrition percentage by salary") +
  ylab("Attrition percentage") +
  xlab("Salary")

# Attrition and salary by education level
dataset %>%
  group_by(Education) %>%
  summarise(Attrition_perc = mean(Attrition == "Yes"),
            Average_salary = mean(MonthlyIncome)) %>%
  ggplot(aes(Education, Attrition_perc, fill = Average_salary)) +
  geom_col(position = "dodge") +
  ggtitle("Attrition and salary by education level") +
  ylab("Attrition percentage")

# Years since last promotion and attrition percentage
dataset %>%
  group_by(YearsSinceLastPromotion) %>%
  summarise(Attrition_perc = mean(Attrition == "Yes")) %>%
  ggplot() +
  geom_col(aes(YearsSinceLastPromotion, Attrition_perc)) +
  scale_y_continuous(name = "Attrition percentage") +
  ggtitle("Years since last promotion and attrition percentage") +
  xlab("Years since last promotion")

# Attrition by department and salary
dataset %>%
  group_by(Attrition, Department) %>%
  summarise(Average_salary = mean(MonthlyIncome)) %>%
  ggplot(aes(Department, Average_salary, fill = Attrition)) +
  geom_col(position = "dodge", colour = "black") +
  ggtitle("Attrition levels by department and salary") +
  ylab("Average salary") +
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_fill_manual(values=c("#00CC00", "#CC0000"))

# Average salary by department and level of education
dataset %>%
  group_by(Education, Department) %>%
  summarise(Average_salary = mean(MonthlyIncome)) %>%
  ggplot(aes(Department, Average_salary, fill = Education)) +
  geom_col(position = "dodge", colour = "black") +
  ggtitle("Average salary by department and level of education") +
  ylab("Average salary") +
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank())

# Attrition percentage by overtime status
dataset %>%
  group_by(OverTime) %>%
  summarise(Attrition_perc = mean(Attrition == "Yes")) %>%
  ggplot(aes(OverTime, Attrition_perc)) +
  geom_col(fill = "#E52B50") +
  ggtitle("Attrition percentage by overtime status") +
  ylab("Attrition percentage") +
  xlab("Overtime status") +
  theme(axis.ticks.x = element_blank())

## Personal factors ##

# Attrition percentage by travelling status
dataset %>%
  group_by(BusinessTravel) %>%
  summarise(Attrition_perc = mean(Attrition == "Yes")) %>%
  mutate(BusinessTravel = reorder(BusinessTravel, Attrition_perc)) %>%
  ggplot(aes(BusinessTravel, Attrition_perc)) +
  geom_col(fill = "#0066CC") +
  ggtitle("Attrition percentage by travelling status") +
  ylab("Attrition percentage") +
  xlab("Travelling status") +
  theme(axis.ticks.x = element_blank())

# Compute attrition regression by distance from home
distanceRegression <- dataset %>%
  group_by(DistanceFromHome) %>%
  summarise(Attrition_perc = mean(Attrition == "Yes")) %>%
  lm(Attrition_perc ~ DistanceFromHome, data = .)

# Attrition levels by distance from home
cols <- c("Attrition interpolation" = "#0000FF",
          "Attrition regression" = "#FF0000")
dataset %>%
  group_by(DistanceFromHome) %>%
  summarise(Attrition_perc = mean(Attrition == "Yes"), Count = n()) %>%
  ggplot() +
  geom_point(aes(DistanceFromHome, Attrition_perc)) +
  geom_smooth(aes(DistanceFromHome, Attrition_perc, colour = "Attrition interpolation")) +
  geom_abline(aes(slope = distanceRegression$coefficients["DistanceFromHome"],
              intercept = distanceRegression$coefficients["(Intercept)"],
              colour = "Attrition regression"), size = 1.2) +
  scale_colour_manual(name="Legend", values=cols) +
  ggtitle("Attrition levels by distance from home") +
  xlab("Distance from home") +
  ylab("Attrition percentage")

# Attrition percentage by marital status
dataset %>%
  group_by(MaritalStatus) %>%
  summarise(Attrition_perc = mean(Attrition == "Yes")) %>%
  ggplot(aes(MaritalStatus, Attrition_perc)) +
  geom_col(fill = "#E52B50") +
  ggtitle("Attrition percentage by marital status") +
  ylab("Attrition percentage") +
  xlab("Marital status") +
  theme(axis.ticks.x = element_blank())

# Marital status by age
dataset %>%
  group_by(MaritalStatus) %>%
  ggplot(aes(MaritalStatus, Age, fill = MaritalStatus)) +
  geom_boxplot() +
  ggtitle("Marital status by age") +
  ylab("Age") +
  xlab("Marital status") +
  theme(axis.ticks.x = element_blank())

################################
# Model building
################################

## Correlation matrix ##

# Get the correlation matrix by first converting factors
# to integer, and then using cor() rounding to the first
# decimal place.
correlationMatrix <- dataset %>% select(-Attrition)
correlationMatrix[,which(sapply(correlationMatrix, class) == "factor")] <-
  lapply(correlationMatrix[,which(sapply(correlationMatrix, class) == "factor")], as.integer)

correlationMatrix <- round(cor(correlationMatrix), 1)

# Plot the lower triangle correlation matrix.
ggcorrplot(correlationMatrix,
           type = "lower",
           colors = c("red", "white", "blue"),
           lab = TRUE,
           lab_size = 2,
           tl.cex=7)

## Feature selection ##

# Feature selection step using filter method
set.seed(1, sample.kind = "Rounding")
filterControl <- sbfControl(functions = rfSBF,
                         method = "boot",
                         number = 10)
featureSelection <- sbf(Attrition ~ .,
                        data = dataset,
                        sbfControl = filterControl)

# Keep only most relevant features
featureSelection$optVariables
dataset <- dataset %>% select(Age, BusinessTravel, DailyRate, Department,
                              DistanceFromHome, EducationField,
                              JobInvolvement, JobLevel, JobRole,
                              JobSatisfaction, MaritalStatus,
                              MonthlyIncome, OverTime, StockOptionLevel,
                              TotalWorkingYears, TrainingTimesLastYear,
                              WorkLifeBalance, YearsAtCompany,
                              YearsInCurrentRole, YearsWithCurrManager,
                              Attrition)

## Model checking ##

# Create the training set (80%) and the validation set (20%)
# from the new dataset. More info in the report.
set.seed(1, sample.kind = "Rounding")
validation_index <- createDataPartition(dataset$Attrition, times = 1, p = 0.2, list = FALSE)
training <- dataset[-validation_index,]
validation <- dataset[validation_index,]

# Create bootstraps training control.
trainingCtrl <- trainControl(method = "boot",
                             number = 20,
                             summaryFunction = twoClassSummary,
                             classProbs = TRUE)

# Define weighs.
# An error on 'Yes' weights more than on 'No'
model_weights <- ifelse(training$Attrition == "Yes",
                        (1 / table(training$Attrition)[[1]]) * 0.5,
                        (1 / table(training$Attrition)[[2]]) * 0.5)

# Defining models to check
models <- c("glm", "bayesglm", "rda",
            "rf", "pcaNNet")

# Define tuning parameters
tuningFrames <- list("rda" =     expand.grid(gamma = seq(0, 1, 0.2),
                                             lambda = seq(0.2, 1, 0.2)),
                     "rf" =      expand.grid(mtry = seq(1, 9, 2)),
                     "pcaNNet" = expand.grid(size = seq(1, 4, 1),
                                             decay = seq(0.1, 1, 0.1)))

# Running model checking phase.
# Using seed for a matter of reproducibility
set.seed(1, sample.kind = "Rounding")
results <- lapply(models, function(mod){
  cat("Model ", mod, "\n")
  fit <- train(Attrition ~ .,
               data = training,
               method = mod,
               tuneGrid = tuningFrames[[mod]],
               trControl = trainingCtrl,
               weights = model_weights,
               metric = "ROC",
               trace = FALSE)
  return(c("Best ROC" = max(fit$results["ROC"]), fit$bestTune))
})
names(results) <- models

################################
# Final results
################################

# Model building based on previous results
set.seed(1, sample.kind = "Rounding")
model <- train(Attrition ~ .,
               data = training,
               weights = model_weights,
               method = "glm")

# Predict attrition
predictions <- predict(model, validation)

# Get and plot the confusion matrix.
cm <- confusionMatrix(predictions, validation$Attrition)
draw_confusion_matrix(cm)

# Get feature importance
featuresImportance <- varImp(model)$importance
featuresImportance[,"Feature"] <- rownames(featuresImportance)
rownames(featuresImportance) <- NULL

# And plot them
featuresImportance %>%
  mutate(Feature = reorder(Feature, Overall)) %>%
  ggplot(aes(Feature, Overall, fill = Feature)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none") +
  ggtitle("Feature importance") +
  ylab("Importance (%)")