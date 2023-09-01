 library(lme4)
 library(lsmeans)
 library(lmerTest)
 library(MuMIn)
 
 ## Set working Directory
 working_directory = "C:/Users/Richy/OneDrive - Duke University/Doctorado/Labs/De Brigard/Projects/CFT_Hippocampus/Paper/Public Data"
 setwd(working_directory)
 
 ## Read data from experiment 1 and 2
 exp1 = read.csv("Exp_1_Plausibility_ratings.csv")
 exp2 = read.csv("Exp_2_Plausibility_ratings.csv")
 
 ## Relation between Plausibility and Detail
 model <- lmer(Plausibility ~  Detail + (1|ID),data=exp1)
 summary(model)
 r.squaredGLMM(model)
 
 ## Relation between Plausibility and difficulty
 model <- lmer(Plausibility ~  Difficulty + (1|ID),data=exp2)
 summary(model)
 r.squaredGLMM(model)
 
