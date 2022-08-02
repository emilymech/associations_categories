# Central Prime N400 Data Visualization (Pre-Analysis)

# modify to local path
data_script_path <- '~OSF/analysis/HLMs/Central/N400/central_N400_data_RR.R'

# load in data
source(data_script_path)

# load in libraries
library(ggplot2)
library(Rmisc)
library(lattice)
library(formattable)

# Data visualization

# Level 1 regressions
amp_VF <- lm(averaged_amp~VF, data=N400_central_wide_st)
conflm1<-confint(amp_VF)
plot(N400_central_wide_st$VF, N400_central_wide_st$averaged_amp, type='p', main='Overall regression of amp~VF with 95% CI')
abline(amp_VF)
abline(coef=conflm1[,1],lty=2)
abline(coef=conflm1[,2],lty=2)

amp_type <- lm(averaged_amp~type, data=N400_central_wide_st)
conflm1<-confint(amp_type)
plot(N400_central_wide_st$type, N400_central_wide_st$averaged_amp, type='p', main='Overall regression of amp~type with 95% CI')
abline(amp_type)
abline(coef=conflm1[,1],lty=2)
abline(coef=conflm1[,2],lty=2)

amp_strength <- lm(averaged_amp~strength, data=N400_central_wide_st)
conflm1<-confint(amp_strength)
plot(N400_central_wide_st$strength, N400_central_wide_st$averaged_amp, type='p', main='Overall regression of amp~strength with 95% CI')
abline(amp_type)
abline(coef=conflm1[,1],lty=2)
abline(coef=conflm1[,2],lty=2)


# Compare mean amplitude by predictors in each Pp
amp.VF <- aggregate(averaged_amp ~ VF + pp_num, data=N400_central_wide_st, FUN="mean")
ggplot(amp.VF, aes(x=VF, y=averaged_amp)) + geom_col() + facet_wrap(~pp_num) + labs(title = "Amp by VF for each Pp") + theme(legend.position="none")

amp.type <- aggregate(averaged_amp ~ type + pp_num, data=N400_central_wide_st, FUN="mean")
ggplot(amp.type, aes(x=type, y=averaged_amp)) + geom_col() + facet_wrap(~pp_num) + labs(title = "Amp by Type for each Pp") + theme(legend.position="none")

amp.strength <- aggregate(averaged_amp ~ strength + pp_num, data=N400_central_wide_st, FUN="mean")
ggplot(amp.strength, aes(x=strength, y=averaged_amp)) + geom_col() + facet_wrap(~pp_num) + labs(title = "Amp by Strength for each Pp") + theme(legend.position="none")


# Variability in predictor and amp relationship
xyplot(averaged_amp ~ VF | pp_num, data=N400_central_wide_st, col.line='black', 
       type=c('p', "r"),
       main='Varability in Amp ~ VF relationship')
xyplot(averaged_amp ~ type | pp_num, data=N400_central_wide_st, col.line='black', 
       type=c('p', "r"),
       main='Varability in Amp ~ type relationship')
xyplot(averaged_amp ~ strength | pp_num, data=N400_central_wide_st, col.line='black', 
       type=c('p', "l"),
       main='Varability in Amp ~ strength relationship')


# Visualize N400 Central

# VF Mean
y <- aggregate(averaged_amp~VF, data=N400_central_wide_st, "mean")

# plot means by uv for VF
plot(y[,1], y[,2], 
     type='b', 
     ylab='Mean uV',
     xlab='VF',
     main='Marginal of uV x VF, N400 Central')

# mean uV by type
y <- aggregate(averaged_amp~type, data=N400_central_wide_st, "mean")

# plot mean uv for type
plot(y[,1], y[,2], 
     type='b', 
     ylab='Mean uV',
     xlab='Relatedness Type',
     main='Marginal of uV x Type, N400 Central')

# plot mean uv by strength
y <- aggregate(averaged_amp~strength, data=N400_central_wide_st, "mean")

# plot meanuv for strength
plot(y[,1], y[,2], 
     type='b', 
     ylab='Mean uV',
     xlab='Relatedness Strength',
     main='Marginal of uV x Strength, N400 Central')

# Summary descriptives
N400_central_wide_st_factored <- N400_central_wide_st
N400_central_wide_st_factored$strength <- revalue(N400_central_wide_st_factored$strength, c("Unrel"="Unrelated"))
N400_central_wide_st_factored$strength <- factor(N400_central_wide_st_factored$strength, levels = c("Strong", "Weak", "Unrelated"))
descriptives <- summarySE(N400_central_wide_st_factored, measurevar = "averaged_amp", groupvars = c("type", "VF", "strength"))

names(descriptives)[names(descriptives)=="strength"] <- "Strength"
names(descriptives)[names(descriptives)=="type"] <- "Type"
names(descriptives)[names(descriptives)=="averaged_amp"] <- "Mean Amplitude"

descriptives$Type <- revalue(descriptives$Type, c("ASS"="Association", "CAT" = "Category"))

pretty_descriptives <- formattable(descriptives, digits=3)
pretty_descriptives

