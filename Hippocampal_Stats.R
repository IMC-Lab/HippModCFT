 library(lme4)
 library(lsmeans)
 library(lmerTest)
 library(reshape2)
 
 ## Set working Directory
 working_directory = ""
 setwd(working_directory)
 
 ## Read data from experiment 1 and 2
 exp1 = read.csv("Exp_1.csv")
 exp2 = read.csv("Exp_2.csv")
 
 ## Z-Score the data to find outliers
 scale((exp1$Left + exp1$Right)/2)
 scale((exp2$Left + exp2$Right)/2)
 
 exp2 = exp2[-14,] # Remove subject 14, which has a Z-score of -2.9
 
 ## Test if the Left and right Hippocampal ROI from the Harvard-Oxford atlas are significantly
 ## different from 0
 
 # Experiment 1
 t.test(exp1$Left)
 t.test(exp1$Right)
 
 # Experiment 2
 t.test(exp2$Left)
 t.test(exp2$Right)
 
 ## Test if the Left and right Hippocampal ROI from the Brainnetome atlas are significantly
 ## different from 0
 
 # Experiment 1 caudal portion
 t.test(exp1$C_Left)
 t.test(exp1$C_Right)
 
 # Experiment 1 rostral portion
 t.test(exp1$R_Left)
 t.test(exp1$R_Right)
 
 # Experiment 2 caudal portion
 t.test(exp2$C_Left)
 t.test(exp2$C_Right)
 
 # Experiment 2 rostral portion
 t.test(exp2$R_Left)
 t.test(exp2$R_Right)
 
 ## Mixed Effect Model 
 # Merge one dataframe for both experiment
 
 exp1$Exp = rep(1,nrow(exp1))
 exp2$Exp = rep(2,nrow(exp2))
 
 exp1$ID = c(1:nrow(exp1))
 exp1$ID = as.factor(exp1$ID)
 
 exp2$ID = c(100:(nrow(exp2)+99))
 exp2$ID = as.factor(exp2$ID)
 
 exp = rbind(exp1,exp2)
 exp = exp[,-c(3:6)]

 exp = melt(exp, id.vars = c('Exp','ID'),measure.vars = c('Left','Right')) 
 colnames(exp) <- c('Experiment','ID','Hemisphere','Estimate')
 
 mixed_model <- lmer(Estimate ~  Hemisphere +  (1 | Experiment/ID),data=exp)
 summary(mixed_model)
