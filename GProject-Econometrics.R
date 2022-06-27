# Clear memory
rm(list = ls())

# Set the working directory
setwd("~/Documents/IÉSEG SCHOOL OF MANAGEMENT/R/ECONOMETRICS R /GProject-ECONOMETRICS")

# Import and load packages here

# install.packages("car")
library(car)
# install.packages("readxl")
library(readxl)
# install.packages("lmtest") # Install package
library(lmtest) # Load library lmtest
# install.packages("sandwitch")
library(sandwich) # Load library sandwich

library(quantmod) # Library to Optimize Returns and Key indicators computation

# Import Excel file 
rv_data <- read_excel("Group Work Data - JL.xlsx")
show(rv_data)

###############################################
# Case 1: The Single Index Model (Weight: 40%)
###############################################
#Define the variables

ExRSP <- rv_data$EXRSP500
ExP1 <- rv_data$EXRET_P1

#Single Index Model
model1 <- lm(rv_data$EXRSP500 ~ ExP1)
summary(model1)

# TSS Computation, 
TSS <- (length(rv_data)-1)*var(rv_data)
print(TSS)

#RSS Computation 
e1hat <- model1$residuals
e1hat_sq <- e1hat*e1hat
RSS <- sum(e1hat_sq)
print(RSS)

#ESS Computation
ESS <- TSS - RSS 
print(ESS)