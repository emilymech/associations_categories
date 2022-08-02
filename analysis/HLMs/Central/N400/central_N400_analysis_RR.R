# Model Fitting

# modify to local path
data_script_path <- '~/OSF/analysis/HLMs/Central/N400/central_N400_data_RR.R'
visualization_script_path <- '~/OSF/analysis/HLMs/Central/N400/central_N400_visualization_RR.R'
function_path <- '~/OSF/src/all_functions.txt'
  
# load in data and visualization
source(data_script_path)
source(visualization_script_path)

# Read in Carolyn Anderson's lmer Functions
source(function_path, local = knitr::knit_global())

# load in libraries
library(lme4)
library(lmerTest)
library(texreg)
library(HLMdiag)
library(tidyverse)
library(ggeffects)
library(sjPlot)

### Null Model
model.null <- lmer(averaged_amp ~ 1 + (1|pp_num) + (1|target_id),
                   data=N400_central_wide_st, REML=FALSE) 
icc(model.null)

### Model building
base <- lmer(averaged_amp ~ 1 + VF + strength + type + (1|pp_num) + (1|target_id),
             data=N400_central_wide_st, REML=FALSE)
anova(model.null, base)

# Now fit a model that includes the predictions of coarse coding hypothesis and type hypothesis
coarse_and_type <- lmer(averaged_amp ~ 1 + VF + strength + type + VF*strength + VF*type + (1|pp_num) + (1|target_id), data=N400_central_wide_st, REML=FALSE)
anova(base, coarse_and_type)

# Now fit a model that tests just the coarse coding hypothesis
coarse_only <- lmer(averaged_amp ~ 1 + VF + strength + type + VF*strength + (1|pp_num) + (1|target_id),
                    data=N400_central_wide_st, REML=FALSE)
anova(base, coarse_only) 

# Now fit a model that tests just the type hypothesis
type_only <- lmer(averaged_amp ~ 1 + VF + strength + type + VF*type + (1|pp_num) + (1|target_id),
                  data=N400_central_wide_st, REML=FALSE)
anova(base, type_only) 

# Test if you need a random slopes are needed
VF_slope <- lmer(averaged_amp ~ 1 + VF + strength + type + (1 + VF|pp_num) + (1|target_id),
                 data=N400_central_wide_st, REML=FALSE)

# get the p value that reflects the mixed chi square distribution
test.table <- (anova(VF_slope, base)) 
p1  <- test.table[2,8]                                 
df0 <- test.table[2,7] - 1
p0  <- pchisq(test.table[2,6],df0,lower.tail=FALSE)    
round(pvalue <- .5*(p1+p0),digits=3)

# make a nice table with the correct p value
test.out <- matrix(c(test.table[2,6],test.table[2,7],df0,p1,p0,pvalue),nrow=1)
test.out <- as.data.frame(test.out)
names(test.out) <- c("LR statistic","df1","df0","p1","p0","pvalue") #LR = likelihood ratio
round(test.out,digits=3) 

# check if we need a type slope
type_slope <- lmer(averaged_amp ~ 1 + VF + strength + type + (1 + type|pp_num) + (1|target_id),
                   data=N400_central_wide_st, REML=FALSE)

# get the p value that reflects the mixed chi square distribution
test.table <- (anova(type_slope, base)) 
p1  <- test.table[2,8]                                 
df0 <- test.table[2,7] - 1
p0  <- pchisq(test.table[2,6],df0,lower.tail=FALSE)    
round(pvalue <- .5*(p1+p0),digits=3)

# make a nice table with the correct p value
test.out <- matrix(c(test.table[2,6],test.table[2,7],df0,p1,p0,pvalue),nrow=1)
test.out <- as.data.frame(test.out)
names(test.out) <- c("LR statistic","df1","df0","p1","p0","pvalue") #LR = likelihood ratio
round(test.out,digits=3) 

# test whether we need a strength slope
strength_slope <-  lmer(averaged_amp ~ 1 + VF + strength + type + (1 + strength|pp_num) + (1|target_id),
                        data=N400_central_wide_st, REML=FALSE) 

# Now that we have the model we like, let's look at some of the contrasts in the best fitting model
summary(base)
# test whether unrelated and weak amplitudes are different
L <- matrix(0,nrow=1,ncol=5)
L[1,3]  <- 1
L[1,4] <- -1
round(contrast(base,L),digits=3)

# Check robust SE
sand.1 <- robust(base, N400_central_wide_st$averaged_amp, N400_central_wide_st$pp_num, df="between/within")
round(sand.1, digits = 2)

# Model Visualization
# plot model predictions N400 central
N400_central_wide_st$strength <- factor(N400_central_wide_st$strength, levels = c("Strong", "Weak", "Unrel"))
N400_central_wide_st$strength <- revalue(N400_central_wide_st$strength, c("Strong"="Strong", "Weak"="Weak", "Unrel"="Unrelated"))
N400_central_wide_st$type <- revalue(N400_central_wide_st$type, c("ASS"="Association", "CAT"="Category"))

theme_new <- theme_set(theme_linedraw())
theme_new <- theme_update(theme_replace(legend.position = "bottom", text = element_text(size=22), axis.line = element_line(colour = "black")))
N400_central_st_model_plot <- ggplot(N400_central_wide_st, aes(strength, averaged_amp, color = VF)) + 
  stat_summary(aes(y=fitted(base)), fun = mean, geom="line", size = 1) + 
  stat_summary(fun.data = mean_se, geom="pointrange", size = 1) +
  facet_wrap(~type) + 
  scale_color_manual(values=c("seagreen2","seagreen4")) + 
  labs(x="\nPrime-Target Relatedness Strength (N400)", y="Predicted Mean Amplitude", color="Visual Field")
N400_central_st_model_plot

