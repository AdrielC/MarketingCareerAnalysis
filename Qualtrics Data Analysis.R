rm(list = ls())
library(ggplot2)
library(dplyr)
library(tidyr)


##################################
########## Data Cleaning #########
##################################

setwd("~/MAIN/School/Intro to Analytics/Final Project/Data/")
data.raw <- read.csv("Wake Forest Case Comp_November 13, 2017_08.17.csv", na.strings=c("",NA))

# Create a list of Question numbers and corresponding text that we can reference later
Questions <- data.raw[1,]

# Remove the first and second rows (not including the header)
data.raw <- data.raw[-c(1:2),]

# Droplevels drops all of the unused levels
# convert all of the numeric data.raw to numeric type
for(i in 1:length(data.raw)){
  ifelse(grepl("Q2_", colnames(data.raw)[i]), 
         data.raw[,i] <- as.numeric(droplevels(data.raw[,i])), 
         data.raw[,i] <- droplevels(data.raw[,i]))
  if(grepl("Q13_", colnames(data.raw)[i])){
    data.raw[,i] <- as.numeric(droplevels(data.raw[,i]))
  }
  if(grepl("Q16_", colnames(data.raw)[i])){
    data.raw[,i] <- as.numeric(droplevels(data.raw[,i]))
  }
  if(grepl("Q17_", colnames(data.raw)[i])){
    data.raw[,i] <- as.numeric(droplevels(data.raw[,i]))
  }
  if(grepl("Q14_", colnames(data.raw)[i])){
    data.raw[,i] <- as.numeric(droplevels(data.raw[,i]))
  }
  if(grepl("Q15_", colnames(data.raw)[i])){
    data.raw[,i] <- as.numeric(droplevels(data.raw[,i]))
  }
}

str(data.raw)

# Drop all responses that came from the previews, and select only the columns you need

data.clean <- data.raw %>% filter(DistributionChannel == "anonymous") %>% filter(is.na(Q1_11_TEXT)) %>% .[,-c(1:8,10:16)]

# Subset the dataset that contains respondents who opted in for answering more questions.

data2.clean <- data.clean %>% filter(Q8 == "1") %>% filter(!is.na(Q15_15)) %>% filter(is.na(Q11_11_TEXT)) %>% .[,c(59:103,39:57)]

# Subset the first clean dataset to match the second subset
data1 <- data.clean[,c(2,4:38,96:103,39:57)]
data2 <- data2.clean[,-2]

colnames <- c("Job", "R", "SAS", "Python", "HTML.CSS", "Excel", "VBA", "SQL", "MarketingBudgeting",
              "Forecasting", "ABtest", "PricingMdl", "SalesMdl", "Bayesian", "Regression", "Forecasting",
              "Psychology", "SocialSci", "ConsumerDesc", "AdobeCC", "Copywriting", "ContentCreation",
              "Videography", "BrandDesign", "SurveyCreation", "FocusGroups", "ExperimentDesign", "QualitativeAnalysis",
              "SecondaryResearch", "Negotiations", "Communication", "LeadQual", "Prospecting", "CCalling",
              "AccountManagement", "MediaBuying", "Stu-Ind-Uni-Other", "Mark-NonMark-Other", "Industry", "JobLevel", 
              "Professor", "Gender", "DOB", "BYU?", "Similarity", "DataDriven", "SalesFocus", "Lucrative", "Creative",
              "Stable", "Fun", "Prestigious", "Important", "Worthwhile", "Essential", "FastPaced", "Boring", "Grunt", 
              "Unimportant", "Undervalued", "Overvalued", "Underpaid", "Overpaid")

colnames(data2) <- colnames
colnames(data1) <- colnames

levels(data1$Job) <- c("B2B Sales", "General", "Social Media Marketing", "Not Familiar", 
                       "B2C Sales", "Digital Marketing", "Email Marketing", "Data Analyst", 
                       "Brand Strategy", "Content Marketing", "PR", "SEO")

levels(data2$Job) <- c("B2B Sales", "General", "Social Media Marketing", "Not Familiar", 
                      "B2C Sales", "Digital Marketing", "Email Marketing", "Data Analyst", 
                      "Brand Strategy", "Content Marketing", "PR")

##################################
########     Analysis      #######
##################################

library(corrgram)
library(randomForest)

# Merge the data together
fulldata <- rbind(data1, data2)
fulldata <- fulldata[,-10]
corrgram(fulldata)

par(mfrow = c(2,2))
hist(fulldata$R, breaks=7, col="blue")
hist(fulldata$SQL, breaks=7, col="blue")
hist(fulldata$`MediaBuying`, breaks=7, col="blue")
hist(fulldata$`MarketingBudgeting`, breaks=7, col="blue")

skillsData <- fulldata %>% .[,1:35] %>% na.exclude() %>% .[]
rf.survey <- randomForest(Job~.,data=skillsData,ntree=1000,importance=TRUE)
varImpPlot(rf.survey)

# Train algorithm; Change mtry/ntree to optimize random forest
train <- sample(1:nrow(skillsData),round(.7*nrow(skillsData)))
rf.skills <- randomForest(Job~.,data=skillsData,subset=train,ntree=1000,importance=TRUE)

# Get Predictions
pred.skills <- predict(rf.skills,newdata=skillsData[-train,])
actual.skills <- skillsData$Job[-train]
Predictions <- as.data.frame(cbind(actual.skills,pred.skills)) %>% mutate(Result = ifelse(actual.skills==pred.skills, T,F))
Predictions$actual.skills <- as.factor(Predictions$actual.skills)
ggplot(Predictions, aes(x = Result, y = ..count.., fill = Result)) +
  geom_bar() +
  facet_grid(~actual.skills)





