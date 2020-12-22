library(nnet)
library(ggplot2)
library(reshape2)
library(haven)
library(knitr)
library(dplyr)
library(stargazer)
library(effects)
options(scipen = 999, digits = 10)
set.seed(123)

green <- read.csv("input/green_party.csv")
green <- green[,-1]
blue <- read.csv("input/blue_party.csv")
blue <- blue[,-1]
neutral <- read.csv("input/neutral.csv")
neutral <- neutral[,-1]

green$Media_Lean_Obj <- as.factor(green$Media_Lean_Obj)
green$Candidates <- as.factor(green$Candidates)
green$Gender <- as.factor(green$Gender)
green$Education <- as.factor(green$Education)
green <- within(green, Media_Lean_Obj <- relevel(Media_Lean_Obj, ref = "Green"))
green <- within(green, Gender <- relevel(Gender, ref = "female"))
green <- within(green, Candidates <- relevel(Candidates, ref = "Tsai Ing-wen from the DPP"))
green_mod <- multinom(formula = Candidates ~ Gender + Age + as.numeric(as.factor(Marital_Status))
                     + as.numeric(as.factor(Education)) + Frequency + Media_Lean_Obj, data = green)
green_mod_coeff <- as.data.frame(t(exp(coef(green_mod))))


car::linearHypothesis(green_mod,
                      c("Eric Chu from the KMT:Media_Lean_ObjBlue = 0"))
car::linearHypothesis(green_mod,
                      c("James Soong from the PFP:Media_Lean_ObjNeutral = 0"))

green_probs <- predict(green_mod, type = "probs")
predicted.classes <- as.data.frame(ifelse(green_probs > 0.5, "pos", "neg"))

# Interpret the coefficients
zvalues <- summary(green_mod)$coefficients/summary(green_mod)$standard.errors
green_coef_pvalue <- kable(as.data.frame(t(pnorm(abs(zvalues), lower.tail=FALSE)*2)), digits = 5)


blue$Media_Lean_Obj <- as.factor(blue$Media_Lean_Obj)
blue$Candidates <- as.factor(blue$Candidates)
blue$Gender <- as.factor(blue$Gender)
blue <- within(blue, Gender <- relevel(Gender, ref = "female"))
blue <- within(blue, Media_Lean_Obj <- relevel(Media_Lean_Obj, ref = "Blue"))
blue <- within(blue, Candidates <- relevel(Candidates, ref = "Eric Chu from the KMT"))
blue_mod <- multinom(formula = Candidates ~ as.numeric(as.factor(Gender)) + Age + as.numeric(as.factor(Marital_Status))
                      + as.numeric(as.factor(Education)) + Frequency + Media_Lean_Obj, data = blue)
blue_mod_coeff <- as.data.frame(t(exp(coef(blue_mod))))


car::linearHypothesis(blue_mod,
                      c("Tsai Ing-wen from the DPP:Media_Lean_ObjGreen = 0"))
car::linearHypothesis(blue_mod,
                      c("James Soong from the PFP:Media_Lean_ObjNeutral = 0"))

blue_prob <- predict(blue_mod, type = "prob")
blue_predict <- as.data.frame(ifelse(blue_prob > 0.5, "pos", "neg"))

blue_r2 <- mnlfit(blue_mod)$result[2]

zvalues <- summary(blue_mod)$coefficients/summary(blue_mod)$standard.errors
blue_coef_pvalue <- kable(as.data.frame(t(pnorm(abs(zvalues), lower.tail=FALSE)*2)), digits = 5)

# neutral
neutral$Media_Lean_Obj <- as.factor(neutral$Media_Lean_Obj)
neutral$Candidates <- as.factor(neutral$Candidates)
neutral$Gender <- as.factor(neutral$Gender)
neutral <- within(neutral, Gender <- relevel(Gender, ref = "female"))
neutral <- within(neutral, Media_Lean_Obj <- relevel(Media_Lean_Obj, ref = "Neutral"))
neutral <- within(neutral, Candidates <- relevel(Candidates, ref = "James Soong from the PFP"))
neutral_mod <- multinom(formula = Candidates ~ Gender + Age + as.numeric(as.factor(Marital_Status))
                      + as.numeric(as.factor(Education)) + Frequency + Media_Lean_Obj, data = neutral, model = TRUE)
neutral_mod_coeff <- as.data.frame(t(exp(coef(neutral_mod))))

car::linearHypothesis(neutral_mod,
                      c("Tsai Ing-wen from the DPP:Media_Lean_ObjGreen = 0"))

car::linearHypothesis(neutral_mod,
                      c("Eric Chu from the KMT:Media_Lean_ObjBlue = 0"))

neutral_prob <- predict(neutral_mod, type = "prob")
neutral_predict <- as.data.frame(ifelse(neutral_prob > 0.5, "pos", "neg"))

neutral_r2 <- mnlfit(neutral_mod)$result[2]

stargazer(green_mod, type = "text")
stargazer(blue_mod, type = "text")
stargazer(neutral_mod, type = "text")

