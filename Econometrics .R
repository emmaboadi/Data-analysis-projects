## Assignment Two
# Created By: Emmanuel Boadi
# Candidate Number: 113383

#loading the necessary libraries 
library(AER)
library(plm)
library(ggplot2)
library(stargazer)

# Clearing working environment
rm(list = ls())

#Setting working directory
setwd("~/Documents")

# Question 1
#Loading company csv file from working directory
data1 <- read.csv('companydata.csv')
data1 <- na.omit(data1)

#1a.
set.seed(3383)

# Creating a vector of years in the company data 
year <- c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015)

#Randomly selecting a year between 2006 and 2015 
sample(year, 1)

#Creating a subset of the company data set with year equal to the randomly selected year
# and labeling it data2
data2 <- subset(data1, year == 2006)

#1b.
# Estimating a linear regression model of output as a function of employment and capital
reg1 <- lm(output ~ employment + capital, data2)
summary(reg1)

# Finding the heteroskedasticity reboust standard error of the regression model(reg1) 
coeftest(reg1, vcov. = vcovHC, type = 'HC1')

#Question 1c.
#Estimating a regression model of the log of output as a function of the log employment and log capital
reg2 <- lm(log(output) ~ log(employment) + log(capital), data = data2)
summary(reg2)

# Finding the heteroskedasticity reboust standard error of the regression model(reg2)
coeftest(reg2, vcov. = vcovHC, type = 'HC1')

# 1d.
# estimating a regression model to measure relative effect of employment and capital on output
reg3 <- lm(output ~ employment + capital + I(employment * capital), data = data2)
summary(reg3)

#Finding the heteroskedasticity reboust standard error of reg3
coeftest(reg3, vcov. = vcovHC, type = 'HC1')



#Question 2

#2a
# creating a dummy variable, highRD, if R&D is greater than or equal to median R&D and 0 if otherwise 
data2$highRD <- ifelse(data2$research >= median(data2$research), 1, 0)

#2c
# Estimating a regression model for 2b iii
model1 <- lm(output ~ employment + capital + highRD +I(highRD*capital) + I(highRD*employment), data = data2)
summary(model1)

#Finding the heteroskedasticity reboust standard error of the regression model(model1)
coeftest(model1, vcov. = vcovHC, type = 'HC1')

#2e.
# Performing a joint hypothesis testing equality of intercepts between high and low R&D companies 
linearHypothesis(model1, c('highRD = 0'),
                 vcov. = vcovHC, type = 'HC1')

# Performing a joint hypothesis testing equality of slopes coefficients between high and low R&D companies
null <- c('I(highRD * capital)', 'I(highRD * employment)')
linearHypothesis(model1, null,
                 vcov. = vcovHC, type = 'HC1')

# Performing a joint hypothesis, testing equality of all coefficients between high and low R&D companies
linearHypothesis(model1, c('highRD = 0', 'I(highRD * employment) = 0', 'I(highRD * capital) = 0'),
                 vcov. = vcovHC(model1, type = 'HC1'))




# Question 3

#Checking whether data is balanced.
is.pbalanced(data1, index = c('id', 'year'))

#3a.
# Creating a new variable as a measure of labour productivity (output per worker)
data1$outputpc <- data1$output/data1$employment

# Plotting the distribution of output per worker (outputpc)
ggplot(data1,
       aes(x = employment, y = output)) + 
  geom_point(aes(x = employment, y= output),
             data = data1) + 
  geom_smooth(method = 'lm',
              formula = y~x,
              data = data1,
              se = FALSE) +
  labs(title = 'Labour Productivity Measure') +
  ylab('SALES (Output)') +
  xlab('WORKERS') +
  theme_bw()

#3b.
# Removing NA values from data
data1 <- na.omit(data1)

#Estimating an entity and time fixed effects model and labelling it outputpc_mod1 
outputpc_mod1 <- plm(log(outputpc) ~ log(age),
             data = data1,
             index = c('id', 'year'),
             model = 'within',
             effect = 'twoways')

summary(outputpc_mod1)

#Estimating the reboust standard error of the plm model(outputpc_mod1)
coeftest(outputpc_mod1, vcov. = vcovHC, type = 'HC1')

# Adding employment variable to outputpc_mod1 plm model
outputpc_mod2 <- plm(log(outputpc) ~ log(age) + employment,
              data = data1,
              index = c('id','year'),
              model = 'within',
              effect = 'twoways')

#Adding capital variable to outputpc_mod2 plm model
outputpc_mod3 <- plm(log(outputpc) ~ log(age) + employment + capital,
              data = data1,
              index = c('id','year'),
              model = 'within',
              effect = 'twoways')

#Adding research variable to outputpc_mod3 plm model
outputpc_mod4 <- plm(log(outputpc) ~ log(age) + employment + capital + import,
               data = data1,
               index = c('id','year'),
               model = 'within',
               effect = 'twoways')
 
#Estimating the robust standard errors of outputpc_mod2, outputpc_mod3 and outputpc_mod4 plm model
robust_se <- list(sqrt(diag(vcovHC(outputpc_mod2, type = 'HC1'))),
                   sqrt(diag(vcovHC(outputpc_mod3, type = 'HC1'))),
                   sqrt(diag(vcovHC(outputpc_mod4, type = 'HC1'))))
 
#Presenting estimates of the above models in stargazer
stargazer(outputpc_mod2, outputpc_mod3, outputpc_mod4,
           digits = 3,
           header = FALSE,
           type = 'text',
           align = TRUE,
           column.sep.width = '0.5pt',
           no.space = TRUE,
           se = robust_se,
           font.size = 'large',
           title = 'Regression results.',
           model.numbers = FALSE,
           dep.var.labels = 'Productivity Levels',
           column.labels = c('model 2', 'model 3', 'model 4'),
           omit.stat = c('f','ser'))
 
