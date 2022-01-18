
############################ BUSSINESS INTELLIGENCE ASSIGNMENT ###############################

#####preparation
#set working directory
setwd("/Users/ngoduong/Desktop/'Business Intelligence'/")

#===================================

#####1. Load the required libraries
install.packages('readr')
install.packages("tidyverse")

library(readxl)   # to read an EXCEL file
library(dplyr)    # for summary statistics and data cleaning
library(corrr)    # for correlation matrix
library(psych)    # for Cronbachs alpha
library(ggplot2)  # for data visiualisation
library(readr)    # read rectangular data
library(tidyverse)# make available in current R session
library(dummies)  # for generating dummy variables for large categorical variables
library(broom)    # for storing fitted values after a regression for plotting
library(mediation)# for mediation

#===================================
#####2. Open dataset and get an overview

###import the data from an EXCEL file
cc <- read_excel("callcentre_10.xlsx")
hrm <- read_excel("HRM_10.xlsx")

###get an overview of the data
glimpse(cc)
glimpse(hrm)
head(cc)
head(hrm)
summary(cc)
summary(hrm)

### get an overview of some key variables
table(cc$costumer_satisfaction)
table(cc$forwarded)
table(hrm$tenure)
table(hrm$gender)

#========================================
#####3. Data preparation
####3a. Data cleaning
#missing values can be ignored in estimations and plots but it still remains in data frame

#===== call centre data =========

colSums(is.na(cc))

###inspect variable time
summary(cc$time)      #no issues

###inspect variable length
glimpse(cc$length)  
summary(cc$length)  #it seems that there is no serious talk took place within 5 seconds
cc <- cc %>%
   arrange(length)
head(cc, n = 30)
cc$length[cc$length < 20] <- NA  #the threshold when you expect a serious call is, in our case, arbitrary

###inspect variable forwarded
summary(cc$forwarded)
cc$fw <- ifelse(cc$forwarded == "forwarded", 1, 0)
table(cc$forwarded, cc$fw)  #show the number of forwarded call, NA meaning these call did not forward to experts

###inspect variable customer_satisfaction
summary(cc$costumer_satisfaction)   #this variable entails a huge number of missing values we need to understand


###inspect variable problem
table(cc$problem)
cc$issue <- ifelse(cc$problem == "complaint", 1, 
                   ifelse(cc$problem == "delivery", 2,
                          ifelse(cc$problem == "return", 3, 4)))
table(cc$problem, cc$issue)
glimpse(cc$issue)
cc$issue <- as.factor(cc$issue)  #assign as factor variable instead of numerical variable 
#it helps to label the categories
cc$issue <- factor(cc$issue, 
                   labels = c("Complaint", "Delivery", "Return", "Technical"))
table(cc$issue)

#======= hrm data ========

colSums(is.na(hrm))

###inspect variable 
summary(hrm)
head(hrm)
summary(hrm$qualification) #no missing value but it has a number of suspicious value
#find and clean all suspicious variables

read_excel('HRM_10.xlsx') -> hrm
type_convert(hrm) -> hrm

hrm$qualification %>% 
   is.na() %>% 
   sum()

# Convert NA to missing data point for qualification:
hrm %>%
   mutate(qualification = case_when(qualification == "NA" ~ NA_character_, TRUE ~ qualification)) -> hrm
hrm %>% 
   mutate(qualification = ifelse(qualification == "20", NA, qualification)) -> hrm
table(hrm$qualification)
hrm$qualification
# Number of missing observations for qualification (and other columns):
lapply(hrm, function(x) {sum(is.na(x))})
colSums(is.na(hrm))
# There are only 6 missing observations for qualification. Thus, we can delete:
hrm %>%
   filter(!is.na(qualification)) -> hrm_df_non_missing
# What does 20 stand for (qualification) :
hrm_df_non_missing %>%
   group_by(qualification) %>%
   count() %>%
   ggplot(aes(qualification, n, fill = qualification)) +
   geom_col()
# And what does 5 stand for (ethnicity):
hrm_df_non_missing %>%
   group_by(ethnicity) %>%
   count()

hrm_df_non_missing[hrm_df_non_missing$ethnicity == 5]  <- NA 
table(hrm$ethnicity)
####3b. Define variables



#=======================================
#####4. Data visualisation
####4a. Performance measurement of the call centre

###The variables effect on customer_satisfaction

## About customer_satisfaction
#fewer categories make the chart easier to grasp
cc$score <- ifelse(cc$costumer_satisfaction >8,1,    
                   ifelse(cc$costumer_satisfaction >6 & cc$costumer_satisfaction <= 8,2,3))
cc$score <- factor(cc$score, labels = c("good", "average", "poor"))

ggplot(cc, aes(x = agent, fill = factor(score))) + geom_bar(position = "fill")
ggsave("costumer_satisfaction acrrording score.png")


## length on costumer_satisfaction
# create agent_level data that show how long the call require in different levels of customer_satisfaction

agent_level <- cc %>% 
   group_by(agent) %>% 
   summarise(call_handling_time = mean(length, na.rm = TRUE), c_satisfaction = mean(costumer_satisfaction, na.rm = TRUE))
head(agent_level)
# Create line chart to show relationship between length and costumer_satisfaction
ggplot(agent_level, aes(x = call_handling_time, y = c_satisfaction)) + geom_point() + geom_smooth()
ggsave("length~costumer_satisfaction.png")
#regression between length~costumer_satisfaction
reg1 <- lm(costumer_satisfaction ~ length, data = cc)
summary(reg1)
tidy_reg1 <- tidy(reg1)
write.csv(tidy_reg1, "costumer_satisfaction ~ length.csv")


## length on costumer_satisfaction
# change agent_level into the data of relationship between waiting time and costumer_satisfaction
agent_level <- cc %>% 
   group_by(agent) %>% 
   summarise(average_waiting_time = mean(waiting, na.rm = TRUE), c_satisfaction = mean(costumer_satisfaction, na.rm = TRUE))
head(agent_level)
# Create line chart to show relationship between length and costumer_satisfaction
ggplot(agent_level, aes(x = average_waiting_time, y = c_satisfaction)) + geom_point() + geom_smooth()
ggsave("waiting~costumer_satisfaction.png")
#regression between waiting~costumer_satisfaction
reg2 <- lm(costumer_satisfaction ~ waiting, data = cc)
summary(reg2)
tidy_reg2 <- tidy(reg2)
write.csv(tidy_reg2, "costumer_satisfaction ~ waiting.csv")
#Find the peak time and wait time
wait_level <- cc %>%
   group_by(time) %>%
   summarise(average_waiting_by_time = mean(waiting, na.rm = TRUE))
wait_level
ggplot(wait_level, aes(x = time, y = average_waiting_by_time)) + geom_line()
ggsave("waiting_time_by_time.png")
#find average satisfaction by time
score_by_time <- cc %>%
   group_by(time) %>%
   summarise(average_satisfaction_by_time = mean(costumer_satisfaction, na.rm = TRUE))
score_by_time
ggplot(score_by_time, aes(x = time, y = average_satisfaction_by_time)) + geom_line()
ggsave("average_satisfaction_by_time.png")


## to understand who answered the customer satisfaction 
cc$nonresponse <- ifelse(cc$costumer_satisfaction != 0,1,0)
cc$nonresponse[is.na(cc$nonresponse)] <- 0
table(cc$nonresponse, cc$costumer_satisfaction, useNA = "ifany")
reg3 <-  lm(nonresponse ~ length + time + problem, data = cc)
summary(reg3)
tidy_reg3 <- tidy(reg3)
write.csv(tidy_reg3, "nonresponse.csv")


## the trade-off between the two performance measures
reg4 <- lm(length ~ costumer_satisfaction + problem, data = cc) 
summary(reg4)
tidy_reg4 <- tidy(reg4)
write.csv(tidy_reg4, "length ~ costumer_satisfaction + problem.csv")




###The call_handling_time and forwarded situation in 4 seperate types of issue

##Compare the total of call in 4 types of issues
ggplot(cc, aes(fill=forwarded, y=forwarded, x=problem)) +
   geom_bar(position="stack", stat="identity")
ggsave("forwarded_situation_of_problems.png")


##The table of forwarded call situation
forwarded_table <- table(cc$problem, cc$forwarded)
forwarded_table
write.csv(forwarded_table, "forwarded_table.csv")


## Trend of total length of call throughout 24 hours
by_time <- cc %>%
   group_by(time, problem)  %>%
   summarise(total_length = sum(length, na.rm = TRUE))

ggplot(by_time, aes(x = time, y = log(total_length),color = problem, shape = problem)) + geom_line() + 
   theme(legend.position="bottom", legend.title = element_blank()) + ggtitle("total length over time") + labs(y = "log(total length)")
ggsave("length_problem.png")    # this saves a png file at the directory set above



####4b. Assessment of individual and team performance.

#merge 2 files into overall call centre data
data <- merge(cc, hrm, by = "agent")
glimpse(data) 

### Individual performance of the call agent

## Top 10 agent having the best individual performance
# List of top 10 agent having best performance
individual_top <- data %>%
   group_by(agent) %>%
   summarise(average_satisfaction = mean(costumer_satisfaction, na.rm = TRUE)) %>%
   top_n(10,average_satisfaction)
individual_top

# Table of that list
write.csv(individual_top, "top10individual.csv")
# Average score of customer_satisfaction
mean(data$costumer_satisfaction, na.rm = TRUE)

## relationship between the customer_satisfaction and tenure, age
# Regression 
reg5 <- lm( costumer_satisfaction ~ tenure + age, data = data) 
summary(reg5)
tidy_reg5 <- tidy(reg5)
write.csv(tidy_reg5, "costumer_satisfaction ~ tenure + age.csv")
# Line chart
cs_by_tenure <- data %>%
   group_by(agent)  %>%
   summarise(average_customer_satisfaction = mean(costumer_satisfaction, na.rm = TRUE), by_tenure = mean(tenure))
ggplot(cs_by_tenure, aes(x = by_tenure, y = average_customer_satisfaction)) + geom_point() + geom_smooth()
ggsave("cs_by_tenure.png")

## Relationship between customer satisfaction and tenure with length as a mediating variable

reg6a <- lm(costumer_satisfaction ~ tenure, data = data)
summary(reg6a)
tidy_reg6a <- tidy(reg6a)
write.csv(tidy_reg6a, "reg6a.csv")

reg6b <- lm(length ~ tenure, data = data)
summary(reg6b)
tidy_reg6b <- tidy(reg6b)
write.csv(tidy_reg6b, "reg6b.csv")

reg6c <- lm(costumer_satisfaction ~ length + tenure, data = data)
summary(reg6c)
tidy_reg6c <- tidy(reg6c)
write.csv(tidy_reg6c, "reg6c.csv")

library(mediation)
mediation <- mediate(reg6b, reg6c,  treat = "tenure", mediator = "length", na.rm = TRUE)
summary(mediation)


### Team performance of the call agents
## Gender composition 
# Investigate whether the effect of call duration on customer satisfaction depends on gender
gender_dummy<-dummy(data$gender,sep=".")
mod_data<-cbind(data,gender_dummy)
mod_data$length_center<- c(scale(mod_data$length, center=TRUE, scale=FALSE))
reg_mod <- lm(costumer_satisfaction ~ length_center * gender.male, data = mod_data)
summary(reg_mod)
tidy_regmod <- tidy(reg_mod)
write.csv(tidy_regmod, "mod_gender.csv")

reg_mod_pred <- broom::augment(reg_mod)
head(reg_mod_pred)

reg_mod_pred$gender.male <- factor(reg_mod_pred$gender.male, levels = c(0,1), labels = c("men", "women"))
ggplot(reg_mod_pred, aes(x = length_center, y = .fitted)) + geom_line(aes(color = gender.male)) + geom_vline(xintercept = 0) +
   labs(x = "length", y = "satisfaction") + theme(legend.position = "bottom", 
                                                  legend.title = element_blank()) + annotate(geom = "text", x = -0.3, y = 2.9, size = 3, label="Intercept line")
ggsave("reg_mod_gender.png")

# Visualize the relationship by gender between lenth and customer_satisfaction
level_gender <- data %>%
   group_by(agent, gender) %>%
   summarise(average_call_length = mean(length, na.rm = TRUE), average_customer_satisfaction = mean(costumer_satisfaction, na.rm = TRUE))

ggplot(level_gender, aes(x = average_call_length, y = average_customer_satisfaction, color = gender)) + geom_point() + geom_smooth(method ="lm")
ggsave("length_on_gender.png")

# Visualize gender composition in resolving problem
problem_gender <- data %>%
   group_by(problem, gender) %>%
   summarise(average_customer_satisfaction = mean(costumer_satisfaction, na.rm = TRUE))

ggplot(problem_gender, aes(fill=gender, y=average_customer_satisfaction, x=problem)) +
   geom_bar(position="dodge", stat="identity")
ggsave("problem_on_gender.png")


## Qualification composition 
# The average score of customer satisfaction of each qualification
level_qualification <- data %>%
   group_by(qualification) %>%
   summarise(average_customer_satisfaction = mean(costumer_satisfaction, na.rm = TRUE))
ggplot(level_qualification, aes(fill=average_customer_satisfaction, y=average_customer_satisfaction, x=qualification, binwidth = 1)) +
   geom_bar(position = "dodge", stat="identity") + coord_flip()
ggsave("level_qualification.png")

# work productivity of each qualification in each problem
problem_qualification <- data %>%
   group_by(problem, qualification) %>%
   summarise(average_customer_satisfaction = mean(costumer_satisfaction, na.rm = TRUE))

ggplot(problem_qualification, aes(fill=qualification, y=average_customer_satisfaction, x=problem)) +
   geom_bar(position="stack", stat="identity")
ggsave("problem_on_qualification.png")

#dummies of qualification
dum_quali <- ifelse(data$qualification =="apprenticeship", 1, 
                    ifelse(data$qualification =="some college",2, 
                           ifelse(data$qualification =="university",3,
                                  ifelse(data$qualification =="", NA, 4))))

dum_quali <- as.factor(dum_quali)
dum_quali <- factor(dum_quali, labels = c(": apprenticeship", ": some college", ": university",": NA"))

test_dummy1<- dummy(dum_quali,sep=".")
data_with_dummies <- cbind(data,test_dummy1)
quali_reg <- lm(formula = costumer_satisfaction ~ length + waiting + dum_quali.1 + 
                   dum_quali.2 + dum_quali.3 + dum_quali.NA, data = data_with_dummies)
summary(quali_reg)
tidy_reg_quali <- tidy(quali_reg)
write.csv(tidy_reg_quali, "dum_quali.csv")




