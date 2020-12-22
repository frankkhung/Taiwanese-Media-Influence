#### Preamble ####
# Purpose: Data Cleaning for the TCS survey
# Author: Chien-Che Hung
# Data: 22 October 2020
# Contact: chienche.hung@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - you need to have all the files produced from teh data_cleaning file

# load the libraryies
library(nnet)
library(ggplot2)
library(reshape2)
library(haven)
library(knitr)
library(dplyr)
library(stargazer)
library(effects)
# set the digit numbers for the results
options(scipen = 999, digits = 10)
# set the seeds for reproducibility
set.seed(123)

# Import files
green <- read.csv("input/green_party.csv")
green <- green[,-1]
blue <- read.csv("input/blue_party.csv")
blue <- blue[,-1]
neutral <- read.csv("input/neutral.csv")
neutral <- neutral[,-1]

# Green Party Model
# change variables into factors
green$Media_Lean_Obj <- as.factor(green$Media_Lean_Obj)
green$Candidates <- as.factor(green$Candidates)
green$Gender <- as.factor(green$Gender)
green$Education <- as.factor(green$Education)
green <- within(green, Media_Lean_Obj <- relevel(Media_Lean_Obj, ref = "Green"))
green <- within(green, Gender <- relevel(Gender, ref = "female"))
green <- within(green, Candidates <- relevel(Candidates, ref = "Tsai Ing-wen from the DPP"))
# multinomial logistic model from green data
green_mod <- multinom(formula = Candidates ~ Gender + Age + as.numeric(as.factor(Marital_Status))
                     + as.numeric(as.factor(Education)) + Frequency + Media_Lean_Obj, data = green)
# exponentiated the coefficients
green_mod_coeff <- as.data.frame(t(exp(coef(green_mod))))

# check the linearlity hypothesis
car::linearHypothesis(green_mod,
                      c("Eric Chu from the KMT:Media_Lean_ObjBlue = 0"))
car::linearHypothesis(green_mod,
                      c("James Soong from the PFP:Media_Lean_ObjNeutral = 0"))


# change variables into factors
blue$Media_Lean_Obj <- as.factor(blue$Media_Lean_Obj)
blue$Candidates <- as.factor(blue$Candidates)
blue$Gender <- as.factor(blue$Gender)
# set the reference category
blue <- within(blue, Gender <- relevel(Gender, ref = "female"))
blue <- within(blue, Media_Lean_Obj <- relevel(Media_Lean_Obj, ref = "Blue"))
blue <- within(blue, Candidates <- relevel(Candidates, ref = "Eric Chu from the KMT"))
# multinomial logistic regression for the blue party data
blue_mod <- multinom(formula = Candidates ~ as.numeric(as.factor(Gender)) + Age + as.numeric(as.factor(Marital_Status))
                      + as.numeric(as.factor(Education)) + Frequency + Media_Lean_Obj, data = blue)
# exponentiated the coefficients
blue_mod_coeff <- as.data.frame(t(exp(coef(blue_mod))))


# check the linearity hypothesis
car::linearHypothesis(blue_mod,
                      c("Tsai Ing-wen from the DPP:Media_Lean_ObjGreen = 0"))
car::linearHypothesis(blue_mod,
                      c("James Soong from the PFP:Media_Lean_ObjNeutral = 0"))

# neutral
# change variables into factors
neutral$Media_Lean_Obj <- as.factor(neutral$Media_Lean_Obj)
neutral$Candidates <- as.factor(neutral$Candidates)
neutral$Gender <- as.factor(neutral$Gender)
# set the reference category
neutral <- within(neutral, Gender <- relevel(Gender, ref = "female"))
neutral <- within(neutral, Media_Lean_Obj <- relevel(Media_Lean_Obj, ref = "Neutral"))
neutral <- within(neutral, Candidates <- relevel(Candidates, ref = "James Soong from the PFP"))
# multinomial logistic model for neutral data
neutral_mod <- multinom(formula = Candidates ~ Gender + Age + as.numeric(as.factor(Marital_Status))
                      + as.numeric(as.factor(Education)) + Frequency + Media_Lean_Obj, data = neutral, model = TRUE)
# exponentiated coefficients
neutral_mod_coeff <- as.data.frame(t(exp(coef(neutral_mod))))

# check the linearity hypothesis
car::linearHypothesis(neutral_mod,
                      c("Tsai Ing-wen from the DPP:Media_Lean_ObjGreen = 0"))

car::linearHypothesis(neutral_mod,
                      c("Eric Chu from the KMT:Media_Lean_ObjBlue = 0"))


