# Lat N400 Analysis

# modify to local path
data_script_path <- '~/OSF/analysis/HLMs/Lateral/N400/lat_N400_data_RR.R'
visualization_path <- '~/OSF/analysis/HLMs/Lateral/N400/lat_N400_visualization_RR.R'
function_path <- '~/OSF/src/all_functions.txt'
  
# load in data and visualization
source(data_script_path)
source(visualization_path)

# Read in Carolyn Anderson's lmer Functions
source(function_path, local = knitr::knit_global())

# load in libraries
library(lme4)
library(lmerTest)
library(texreg)
library(HLMdiag)
library(tidyverse)
library(sjPlot)

# Model Fitting

# N400 lat null models
# Null Model
model.null <- lmer(averaged_amp ~ 1 + (1|pp_num) + (1|target_id),
                   data=N400_lat_wide_st, REML=FALSE) 
icc(model.null)

# N400 lat st models
base <- lmer(averaged_amp ~ 1 + VF + strength + type + (1|pp_num) + (1|target_id),
             data=N400_lat_wide_st, REML=FALSE, control = lmerControl(optimizer ="Nelder_Mead"))

anova(model.null, base)

# Now fit a model that includes the predictions of coarse coding hypothesis and type hypothesis
coarse_and_type <- lmer(averaged_amp ~ 1 + VF + strength + type + VF*strength + VF*type + (1|pp_num) + (1|target_id),
                        data=N400_lat_wide_st, REML=FALSE, control = lmerControl(optimizer ="Nelder_Mead"))

anova(base, coarse_and_type)

# Now fit a model that tests just the coarse coding hypothesis
coarse_only <- lmer(averaged_amp ~ 1 + VF + strength + type + VF*strength+ (1|pp_num) + (1|target_id),
                    data=N400_lat_wide_st, REML=FALSE, control = lmerControl(optimizer ="Nelder_Mead"))

anova(coarse_and_type, coarse_only) 

# Now fit a model that tests just the type hypothesis
type_only <- lmer(averaged_amp ~ 1 + VF + strength + type + VF*type + (1|pp_num) + (1|target_id),
                  data=N400_lat_wide_st, REML=FALSE, control = lmerControl(optimizer ="Nelder_Mead"))

anova(type_only, coarse_and_type)
anova(coarse_only, type_only)

## Now let's test if we need random slopes 
coarse_VF_slope <-  lmer(averaged_amp ~ 1 + VF + strength + type + VF*strength + (1+VF|pp_num) + (1|target_id),
                         data=N400_lat_wide_st, REML=FALSE, control = lmerControl(optimizer ="Nelder_Mead"))

# get the right RE p value from a mixture chi square distribution
test.table <- (anova(coarse_VF_slope, coarse_only))  
p1  <- test.table[2,8]                                 
df0 <- test.table[2,7] - 1
p0  <- pchisq(test.table[2,6],df0,lower.tail=FALSE)    
round(pvalue <- .5*(p1+p0),digits=3)

# get a table output of the correct p value
test.out <- matrix(c(test.table[2,6],test.table[2,7],df0,p1,p0,pvalue),nrow=1)
test.out <- as.data.frame(test.out)
names(test.out) <- c("LR statistic","df1","df0","p1","p0","pvalue") #LR = likelihood ratio
round(test.out,digits=3) 

# test whether type slope needed
coarse_type_slope <-  lmer(averaged_amp ~ 1 + VF + strength + type + VF*strength + (1+type|pp_num) + (1|target_id),
                           data=N400_lat_wide_st, REML=FALSE, control = lmerControl(optimizer ="Nelder_Mead")) 

# test whether strength slope needed
coarse_strength_slope <- lmer(averaged_amp ~ 1 + VF + strength + type + VF*strength + (1+strength|pp_num) + (1|target_id),
                              data=N400_lat_wide_st, REML=FALSE, control = lmerControl(optimizer ="Nelder_Mead")) 

# Now that we have the model we like, let's look at some of the contrasts in the best fitting model
summary(coarse_VF_slope)  
# the 3rd fixed effect is unrelated, 4th is weak
L <- matrix(0,nrow=1,ncol=7) # 7 is the total number of fixed effects
L[1,3]  <- 1
L[1,4] <- -1
round(contrast(coarse_VF_slope, L),digits=3)

L <- matrix(0,nrow=1,ncol=7) # 7 is the total number of fixed effects
L[1,6]  <- 1
L[1,7] <- -1
round(contrast(coarse_VF_slope, L),digits=3)

# Let's test to make sure that our random effects structure is not misspecified
sand.1 <- robust(coarse_VF_slope, N400_lat_wide_st$averaged_amp, N400_lat_wide_st$pp_num, df="between/within")
round(sand.1, digits = 2) 

sand.2 <- robust(coarse_only, N400_lat_wide_st$averaged_amp, N400_lat_wide_st$pp_num, df="between/within")
round(sand.2, digits = 2) 


# Model Visualization
# plot model predictions N400 lat
N400_lat_wide_st$strength <- factor(N400_lat_wide_st$strength, levels = c("Strong", "Weak", "Unrel"))
N400_lat_wide_st$strength <- revalue(N400_lat_wide_st$strength, c("Strong"="Strong", "Weak"="Weak", "Unrel"="Unrelated"))
N400_lat_wide_st$type <- revalue(N400_lat_wide_st$type, c("ASS"="Association", "CAT"="Category"))

theme_new <- theme_set(theme_linedraw())
theme_new <- theme_update(theme_replace(legend.position = "bottom", text = element_text(size=22), axis.line = element_line(colour = "black")))
N400_lat_wide_st_plot <- ggplot(N400_lat_wide_st, aes(strength, averaged_amp, color = VF)) + 
  stat_summary(aes(y=fitted(coarse_VF_slope)), fun = mean, geom="line", size = 1) + 
  stat_summary(fun.data = mean_se, geom="pointrange", size = 1) + 
  facet_wrap(~type) + 
  scale_color_manual(values=c("seagreen2","seagreen4")) + 
  labs(x="\nPrime-Target Relatedness Strength (N400)", y="Predicted Mean Amplitude", color="Visual Field")
N400_lat_wide_st_plot
