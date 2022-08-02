# Lat LPC Analysis

# modify to local path
data_script_path <- '~/OSF/analysis/HLMs/Lateral/LPC/lat_LPC_data_RR.R'
visualization_script_path <- '~/OSF/analysis/HLMs/Lateral/LPC/lat_LPC_visualization_RR.R'
function_path <- '~/OSF/src/all_functions.txt'
  
# load in data and visualization
source(data_script_path)
source(visualization_script_path)

# Read in Carolyn Anderson's lmer Functions
source(function_path, local = knitr::knit_global())

# Load in libraries
library(lme4)
library(lmerTest)
library(texreg)
library(HLMdiag)
library(tidyverse)
library(sjPlot)

# Model Fitting

# LPC lat null models
# Null Model
model.null <- lmer(averaged_amp ~ 1 + (1|pp_num) + (1|target_id),
                   data=LPC_lat_wide_st, REML=FALSE) 
icc(model.null)

### Model building
base <- lmer(averaged_amp ~ 1 + VF + strength + type + (1|pp_num) + (1|target_id),
             data=LPC_lat_wide_st, REML=FALSE, control = lmerControl(optimizer ="Nelder_Mead"))
anova(model.null, base)

coarse_and_type <- lmer(averaged_amp ~ 1 + VF + strength + type + VF*strength + VF*type + (1|pp_num) + (1|target_id),
                        data=LPC_lat_wide_st, REML=FALSE, control = lmerControl(optimizer ="Nelder_Mead"))

anova(base, coarse_and_type)

coarse_only <- lmer(averaged_amp ~ 1 + VF + strength + type + VF*strength + (1|pp_num) + (1|target_id),
                    data=LPC_lat_wide_st, REML=FALSE, control = lmerControl(optimizer ="Nelder_Mead"))

anova(coarse_and_type, coarse_only)

type_only <- lmer(averaged_amp ~ 1 + VF + strength + type + VF*type + (1|pp_num) + (1|target_id),
                  data=LPC_lat_wide_st, REML=FALSE, control = lmerControl(optimizer ="Nelder_Mead"))

anova(coarse_and_type, type_only)

# Test if random slopes are needed
coarse_type_VF_slope <-  lmer(averaged_amp ~ 1 + VF + strength + type + VF*strength + VF*type + (1+VF|pp_num) + (1|target_id),
                              data=LPC_lat_wide_st, REML=FALSE, control = lmerControl(optimizer ="Nelder_Mead"))

# get the p value for the RE from a mixture chi square distribution
test.table <- (anova(coarse_type_VF_slope, coarse_and_type))  
p1  <- test.table[2,8]                                 
df0 <- test.table[2,7] - 1
p0  <- pchisq(test.table[2,6],df0,lower.tail=FALSE)    
round(pvalue <- .5*(p1+p0),digits=3)

# p value for RE in table
test.out <- matrix(c(test.table[2,6],test.table[2,7],df0,p1,p0,pvalue),nrow=1)
test.out <- as.data.frame(test.out)
names(test.out) <- c("LR statistic","df1","df0","p1","p0","pvalue") #LR = likelihood ratio
round(test.out,digits=3)

coarse_type_type_slope <-  lmer(averaged_amp ~ 1 + VF + strength + type + VF*strength + VF*type + (1+type|pp_num) + (1|target_id),
                                data=LPC_lat_wide_st, REML=FALSE, control = lmerControl(optimizer ="Nelder_Mead")) 

# get the p value for the RE from a mixture chi square distribution
test.table <- (anova(coarse_type_type_slope, coarse_and_type))  
p1  <- test.table[2,8]                                 
df0 <- test.table[2,7] - 1
p0  <- pchisq(test.table[2,6],df0,lower.tail=FALSE)    
round(pvalue <- .5*(p1+p0),digits=3)

# p value for RE in table
test.out <- matrix(c(test.table[2,6],test.table[2,7],df0,p1,p0,pvalue),nrow=1)
test.out <- as.data.frame(test.out)
names(test.out) <- c("LR statistic","df1","df0","p1","p0","pvalue") #LR = likelihood ratio
round(test.out,digits=3)

coarse_type_strength_slope <- lmer(averaged_amp ~ 1 + VF + strength + type + VF*strength + VF*type + (1+strength|pp_num) + (1|target_id),
                                   data=LPC_lat_wide_st, REML=FALSE, control = lmerControl(optimizer ="Nelder_Mead"))

# Now that we have the model we like, let's look at some of the contrasts in the best fitting model
summary(coarse_type_VF_slope )  
# the 3rd fixed effect is unrelated, 4th is weak
L <- matrix(0,nrow=1,ncol=8)
L[1,3]  <- 1
L[1,4] <- -1
round(contrast(coarse_type_VF_slope ,L),digits=3)

# test whether interaction effects are different from each other
L <- matrix(0,nrow=1,ncol=8)
L[1,6]  <- 1
L[1,7] <- -1
round(contrast(coarse_type_VF_slope ,L),digits=3)

# Let's test to make sure that our random effects structure is not misspecified
sand.1 <- robust(coarse_type_VF_slope, LPC_lat_wide_st$averaged_amp, LPC_lat_wide_st$pp_num, df="between/within")
round(sand.1, digits = 2) 

sand.2 <- robust(coarse_and_type, LPC_lat_wide_st$averaged_amp, LPC_lat_wide_st$pp_num, df="between/within")
round(sand.2, digits = 2) 

# plot model predictions LPC lat
LPC_lat_wide_st$strength <- factor(LPC_lat_wide_st$strength, levels = c("Strong", "Weak", "Unrel"))
LPC_lat_wide_st$strength <- revalue(LPC_lat_wide_st$strength, c("Strong"="Strong", "Weak"="Weak", "Unrel"="Unrelated"))
LPC_lat_wide_st$type <- revalue(LPC_lat_wide_st$type, c("ASS"="Association", "CAT"="Category"))

theme_new <- theme_set(theme_linedraw())
theme_new <- theme_update(theme_replace(legend.position = "bottom", text = element_text(size=22), axis.line = element_line(colour = "black")))
LPC_lat_st_model_plot <- ggplot(LPC_lat_wide_st, aes(strength, averaged_amp, color = VF)) + 
  stat_summary(aes(y=fitted(coarse_type_VF_slope)), fun = mean, geom="line", size = 1) + 
  stat_summary(fun.data = mean_se, geom="pointrange", size = 1) + 
  facet_wrap(~type) + 
  scale_color_manual(values=c("royalblue1","darkblue")) + 
  labs(x="\nPrime-Target Relatedness Strength (LPC)", y="", color="Visual Field")
LPC_lat_st_model_plot
