"
Notes:

N400
electrodes: 12:LMFr, 13:RMFr, 16:LMCe, 17:RMCe, 20:MiCe, 21:MiPa, 24:LDPa, 25:RDPa
time window: 300-500

LPC
electrodes: 16:LMCe, 17:RMCe, 20:MiCe, 21:MiPa, 24:LDPa, 25:RDPa, 28:LMOc, 29:RMOc, 30:MiOc,
time window: 600-900

Correct is judged loosely based on whether their intuitions matched ours

"
library(ez)
library(Rmisc)
library(ggplot2)
library(reshape2)
library(psych)
library(apaTables)
library(BayesFactor)

# modify to your local path
N4_path <- "~/OSF/data/Med300-500_central.dat"
LPC_path <- "~OSF/data/Med600-900_central.dat"
  
#  Read in Centralprime Data
N400_central <- read.table(N4_path, header = F) 

LPC_central <- read.table(LPC_path, header = F)

# Central: N400 Add Headers, Rename, Reorder Columns
#  .dat files are 1 column without headers
#  Good example of how the .dat files will be ordered: http://kutaslab.ucsd.edu/erpmanpages/merp_manual.7.html
#  e.g., bin 1 channel 1 for Pp1, bin 1 channel 1 for Pp2...
#  e.g., bin 1 chan 2 for Pp1, bin 1 chan 2 for Pp2...
#  e.g., bin 2 chan 1 for Pp1, bin 2 chan 1 for Pp2...

#  N400 Central:
describe(N400_central) # gives the number of rows to do the rep math

#  rename V1 to something more descriptive
colnames(N400_central)[colnames(N400_central)=="V1"] <- "mean_amp"

#  add a column for Pp that repeats the Pp # (from .mcf) for the whole dataset
N400_central$Pp <- rep(c("11", "9", "2", "1", "4", "8", 
                         "12", "17", "14", "16", "18", "20", 
                         "13", "7", "5", "6", "10", "19", 
                         "15", "21", "3", "22", "23", "24"), times = 192)


#  add a column for chan # 
N400_central$chan <- rep(c("12", "13", "16", "17", "20", "21", "24", "25"), each = 24, times = 24)

#  add a column for chan label
N400_central$chan_label <- ifelse(N400_central$chan == "12", "LMFr",
                                  ifelse(N400_central$chan == "13", "RMFr",
                                         ifelse(N400_central$chan == "16", "LMCe", 
                                                ifelse(N400_central$chan == "17", "RMCe", 
                                                       ifelse(N400_central$chan == "20", "MiCe", 
                                                              ifelse(N400_central$chan == "21", "MiPa", 
                                                                     ifelse(N400_central$chan == "24", "LDPa", 
                                                                            ifelse(N400_central$chan == "25", "RDPa", 
                                                                                   "Houston, you have a problem"))))))))


#  add a column for bin #
N400_central$bin <- rep(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
                          22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33), each = 192)

# add a column for bin label
#  The correct (at least correct with our intuitions...) trials are trials in which the behavioral response was correct either before the prompt or after the prompt, but
#  the bin labels have been abbreviated for convenience
N400_central$bin_label <- ifelse(N400_central$bin == 1, "RVF-Ass-Strong", 
                                 ifelse(N400_central$bin == 2, "LVF-Ass-Strong", 
                                        ifelse(N400_central$bin == 3, "RVF-Ass-Weak", 
                                               ifelse(N400_central$bin == 4, "LVF-Ass-Weak", 
                                                      ifelse(N400_central$bin == 5, "RVF-Ass-Unrel", 
                                                             ifelse(N400_central$bin == 6, "LVF-Ass-Unrel", 
                                                                    ifelse(N400_central$bin == 7, "RVF-Cat-Strong", 
                                                                           ifelse(N400_central$bin == 8, "LVF-Cat-Strong", 
                                                                                  ifelse(N400_central$bin == 9, "RVF-Cat-Weak", 
                                                                                         ifelse(N400_central$bin == 10, "LVF-Cat-Weak", 
                                                                                                ifelse(N400_central$bin == 11, "RVF-Cat-Unrel", 
                                                                                                       ifelse(N400_central$bin == 12, "LVF-Cat-Unrel",
                                                                                                              ifelse(N400_central$bin == 22, "Correct RVF-Ass-Strong", 
                                                                                                                     ifelse(N400_central$bin == 23, "Correct LVF-Ass-Strong", 
                                                                                                                            ifelse(N400_central$bin == 24, "Correct RVF-Ass-Weak", 
                                                                                                                                   ifelse(N400_central$bin == 25, "Correct LVF-Ass-Weak", 
                                                                                                                                          ifelse(N400_central$bin == 26, "Correct RVF-Ass-Unrel", 
                                                                                                                                                 ifelse(N400_central$bin == 27, "Correct LVF-Ass-Unrel", 
                                                                                                                                                        ifelse(N400_central$bin == 28, "Correct RVF-Cat-Strong", 
                                                                                                                                                               ifelse(N400_central$bin == 29, "Correct LVF-Cat-Strong", 
                                                                                                                                                                      ifelse(N400_central$bin == 30, "Correct RVF-Cat-Weak",
                                                                                                                                                                             ifelse(N400_central$bin == 31, "Correct LVF-Cat-Weak", 
                                                                                                                                                                                    ifelse(N400_central$bin == 32, "Correct RVF-Cat-Unrel", 
                                                                                                                                                                                           ifelse(N400_central$bin == 33, "Correct LVF-Cat-Unrel", 
                                                                                                                                                                                                  "Houston, you have a problem"))))))))))))))))))))))))

#  add a column to group RVF and LVF
N400_central$VF <- ifelse(N400_central$bin == 1 | 
                            N400_central$bin == 3 |
                            N400_central$bin == 5 |
                            N400_central$bin == 7 |
                            N400_central$bin == 9 | 
                            N400_central$bin == 11 |
                            N400_central$bin == 22 | 
                            N400_central$bin == 24 |
                            N400_central$bin == 26 |
                            N400_central$bin == 28 |
                            N400_central$bin == 30 | 
                            N400_central$bin == 32, 
                          "RVF", "LVF")
#  add a column to group by type of relatedness
N400_central$type <- ifelse(N400_central$bin == 1 | 
                              N400_central$bin == 2 |
                              N400_central$bin == 3 |
                              N400_central$bin == 4 |
                              N400_central$bin == 5 | 
                              N400_central$bin == 6 | 
                              N400_central$bin == 22 | 
                              N400_central$bin == 23 |
                              N400_central$bin == 24 |
                              N400_central$bin == 25 |
                              N400_central$bin == 26 | 
                              N400_central$bin == 27, 
                            "ASS", "CAT")

#  add a column to group by strength of stimulus
N400_central$strength <- ifelse(N400_central$bin == 1 | 
                                  N400_central$bin == 2 |
                                  N400_central$bin == 7 |
                                  N400_central$bin == 8 |
                                  N400_central$bin == 22 | 
                                  N400_central$bin == 23 |
                                  N400_central$bin == 28 |
                                  N400_central$bin == 29,
                                "Strong", 
                                ifelse(N400_central$bin == 3|
                                         N400_central$bin == 4|
                                         N400_central$bin == 9|
                                         N400_central$bin == 10|
                                         N400_central$bin == 24|
                                         N400_central$bin == 25|
                                         N400_central$bin == 30|
                                         N400_central$bin == 31, 
                                       "Weak", "Unrelated"))

#  add a column to group by whether the behavioral response was correct or incorrect
N400_central$correctness <- ifelse(N400_central$bin > 21, "Correct", "All")

#  reorder column names for personal sanity
N400_central <- N400_central[c("Pp", "bin", "bin_label", "VF", "type", "strength", "correctness", "chan", "chan_label", "mean_amp")]

#  double check that the data look ok
#  View(N400_central)
#  head(N400_central)
#  colnames(N400_central)

#Central: LPC Add Headers, Rename, Reorder Columns
#  LPC Central:
describe(LPC_central) #; gives the number of rows to do the rep math

#  rename V1 to something more descriptive
colnames(LPC_central)[colnames(LPC_central)=="V1"] <- "mean_amp"

#  add a column for Pp that repeats the Pp # (from .mcf) for the whole dataset
LPC_central$Pp <-  rep(c("11", "9", "2", "1", "4", "8", 
                         "12", "17", "14", "16", "18", "20", 
                         "13", "7", "5", "6", "10", "19", 
                         "15", "21", "3", "22", "23", "24"), times = 216)


#  add a column for chan # 
LPC_central$chan <- rep(c("16", "17", "20", "21", "24", "25", "28", "29", "30"), each = 24, times = 24)

#  add a column for chan label
LPC_central$chan_label <- ifelse(LPC_central$chan == "16", "LMCe", 
                                 ifelse(LPC_central$chan == "17", "RMCe", 
                                        ifelse(LPC_central$chan == "20", "MiCe", 
                                               ifelse(LPC_central$chan == "21", "MiPa", 
                                                      ifelse(LPC_central$chan == "24", "LDPa", 
                                                             ifelse(LPC_central$chan == "25", "RDPa", 
                                                                    ifelse(LPC_central$chan == "28", "LMOc", 
                                                                           ifelse(LPC_central$chan == "29", "RMOc", 
                                                                                  ifelse(LPC_central$chan == "30", "MiOc",
                                                                                         "Houston, you have a problem")))))))))


#  add a column for bin #
LPC_central$bin <- rep(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
                         22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33), each = 216)

# add a column for bin label
#  The correct trials are trials in which the behavioral response was correct either before the prompt or after the prompt, but
#  the bin labels have been abbreviated for convenience
LPC_central$bin_label <- ifelse(LPC_central$bin == 1, "RVF-Ass-Strong", 
                                ifelse(LPC_central$bin == 2, "LVF-Ass-Strong", 
                                       ifelse(LPC_central$bin == 3, "RVF-Ass-Weak", 
                                              ifelse(LPC_central$bin == 4, "LVF-Ass-Weak", 
                                                     ifelse(LPC_central$bin == 5, "RVF-Ass-Unrel", 
                                                            ifelse(LPC_central$bin == 6, "LVF-Ass-Unrel", 
                                                                   ifelse(LPC_central$bin == 7, "RVF-Cat-Strong", 
                                                                          ifelse(LPC_central$bin == 8, "LVF-Cat-Strong", 
                                                                                 ifelse(LPC_central$bin == 9, "RVF-Cat-Weak", 
                                                                                        ifelse(LPC_central$bin == 10, "LVF-Cat-Weak", 
                                                                                               ifelse(LPC_central$bin == 11, "RVF-Cat-Unrel", 
                                                                                                      ifelse(LPC_central$bin == 12, "LVF-Cat-Unrel",
                                                                                                             ifelse(LPC_central$bin == 22, "Correct RVF-Ass-Strong", 
                                                                                                                    ifelse(LPC_central$bin == 23, "Correct LVF-Ass-Strong", 
                                                                                                                           ifelse(LPC_central$bin == 24, "Correct RVF-Ass-Weak", 
                                                                                                                                  ifelse(LPC_central$bin == 25, "Correct LVF-Ass-Weak", 
                                                                                                                                         ifelse(LPC_central$bin == 26, "Correct RVF-Ass-Unrel", 
                                                                                                                                                ifelse(LPC_central$bin == 27, "Correct LVF-Ass-Unrel", 
                                                                                                                                                       ifelse(LPC_central$bin == 28, "Correct RVF-Cat-Strong", 
                                                                                                                                                              ifelse(LPC_central$bin == 29, "Correct LVF-Cat-Strong", 
                                                                                                                                                                     ifelse(LPC_central$bin == 30, "Correct RVF-Cat-Weak",
                                                                                                                                                                            ifelse(LPC_central$bin == 31, "Correct LVF-Cat-Weak", 
                                                                                                                                                                                   ifelse(LPC_central$bin == 32, "Correct RVF-Cat-Unrel", 
                                                                                                                                                                                          ifelse(LPC_central$bin == 33, "Correct LVF-Cat-Unrel", 
                                                                                                                                                                                                 "Houston, you have a problem"))))))))))))))))))))))))

#  add a column to group RVF and LVF
LPC_central$VF <- ifelse(LPC_central$bin == 1 | 
                           LPC_central$bin == 3 |
                           LPC_central$bin == 5 |
                           LPC_central$bin == 7 |
                           LPC_central$bin == 9 | 
                           LPC_central$bin == 11 |
                           LPC_central$bin == 22 | 
                           LPC_central$bin == 24 |
                           LPC_central$bin == 26 |
                           LPC_central$bin == 28 |
                           LPC_central$bin == 30 | 
                           LPC_central$bin == 32, 
                         "RVF", "LVF")
#  add a column to group by type of relatedness
LPC_central$type <- ifelse(LPC_central$bin == 1 | 
                             LPC_central$bin == 2 |
                             LPC_central$bin == 3 |
                             LPC_central$bin == 4 |
                             LPC_central$bin == 5 | 
                             LPC_central$bin == 6 | 
                             LPC_central$bin == 22 | 
                             LPC_central$bin == 23 |
                             LPC_central$bin == 24 |
                             LPC_central$bin == 25 |
                             LPC_central$bin == 26 | 
                             LPC_central$bin == 27, 
                           "ASS", "CAT")

#  add a column to group by strength of stimulus
LPC_central$strength <- ifelse(LPC_central$bin == 1 | 
                                 LPC_central$bin == 2 |
                                 LPC_central$bin == 7 |
                                 LPC_central$bin == 8 |
                                 LPC_central$bin == 22 | 
                                 LPC_central$bin == 23 |
                                 LPC_central$bin == 28 |
                                 LPC_central$bin == 29,
                               "Strong", 
                               ifelse(LPC_central$bin == 3|
                                        LPC_central$bin == 4|
                                        LPC_central$bin == 9|
                                        LPC_central$bin == 10|
                                        LPC_central$bin == 24|
                                        LPC_central$bin == 25|
                                        LPC_central$bin == 30|
                                        LPC_central$bin == 31, 
                                      "Weak", "Unrelated"))

#  add a column to group by whether the behavioral response was correct or incorrect
LPC_central$correctness <- ifelse(LPC_central$bin >21, "Correct", "All")

#  reorder column names for personal sanity
LPC_central <- LPC_central[c("Pp", "bin", "bin_label", "VF", "type", "strength", "correctness", "chan", "chan_label", "mean_amp")]

#  double check that the data look ok
#  View(LPC_central)
#  head(LPC_central)
#  colnames(LPC_central)


# Central Prime

### Central Prime RVF Association Waveforms, -100 - 900ms

# Central Prime, N400 Mean Amps (300-500ms) 
# Central: N400 Visualize Data
#  N400 Central Visualization
N400c_bin_sum <- summarySE(N400_central, measurevar = "mean_amp", groupvars = c("bin_label", "correctness"))
N400c_bin_sum

N400c_grouped_sum <- summarySE(N400_central, measurevar = "mean_amp", groupvars = c("VF", "type", "strength", "correctness"))
N400c_grouped_sum

### Compare N400 mean amplitudes by strength of relatedness (strong, weak, unrelated) of all trials and correct only trials
# N400 mean amp table and plots by bin label
N400c_correct <- summarySE(N400_central, measurevar = "mean_amp", groupvars = c("correctness", "strength"))
N400c_correct

N400c_binplot_all <- ggplot(N400c_bin_sum[N400c_bin_sum$correctness == "All",], aes(x=bin_label, y=mean_amp, fill = bin_label)) + geom_bar(stat="identity") + geom_errorbar(aes(ymin=mean_amp+se, ymax=mean_amp-se), width=.1) + xlab("") + ylab("Mean Amplitude") + ggtitle("N400 Central Prime Mean Amplitudes") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + theme(legend.title = element_blank()) + guides(fill=FALSE)
N400c_binplot_all

N400c_binplot_correct <- ggplot(N400c_bin_sum[N400c_bin_sum$correctness == "Correct",], aes(x=bin_label, y=mean_amp, fill = bin_label)) + geom_bar(stat="identity") + geom_errorbar(aes(ymin=mean_amp+se, ymax=mean_amp-se), width=.1) + xlab("") + ylab("Mean Amplitude") + ggtitle("N400 Central Prime Mean Amplitudes, Before and After Cue Correct Trials Only") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + theme(legend.title = element_blank()) + guides(fill=FALSE)
N400c_binplot_correct

# N400: Make data with a superelectrode
N400_central_super_all <- dcast(N400_central[N400_central$correctness == "All",], Pp + bin + bin_label + VF + type + strength ~ correctness, value.var="mean_amp", fun.aggregate = mean)
summarySE(N400_central_super_all, measurevar = "All", groupvars = c("VF", "type"))

#N400: Make data by Type
N400_central_super_all_ass <- N400_central_super_all[N400_central_super_all$type == "ASS",]
N400_central_super_all_cat <- N400_central_super_all[N400_central_super_all$type == "CAT",]

#Check var assumptions
ggplot(N400_central_super_all_ass, aes(x=All)) + geom_histogram()
ggplot(N400_central_super_all_cat, aes(x=All)) + geom_histogram()

### Central Prime N400 Association Data ANOVA
# Central: N400 ASS ANOVAs
#  N400 Central ANOVA for all trials
N400_central_super_all_ass$strength <- factor(N400_central_super_all_ass$strength)
N400_central_super_all_ass$VF <- factor(N400_central_super_all_ass$VF)
N400_central_super_all_ass$Pp <- factor(N400_central_super_all_ass$Pp)

N400_central_ass_ez <- ezANOVA(data = N400_central_super_all_ass, dv = All, wid = Pp, within = .(strength, VF))
apa.ezANOVA.table(
  N400_central_ass_ez,
  correction = "GG",
  table.title = "",
  table.number = NA
)

N4_cen_ass_bfMainEffects = lmBF(All ~ strength + VF, data =  N400_central_super_all_ass)
N4_cen_ass_bfInteraction = lmBF(All ~ strength + VF + strength:VF, data = N400_central_super_all_ass)

## Compare the two models
N4_cen_ass_bf = N4_cen_ass_bfInteraction / N4_cen_ass_bfMainEffects
N4_cen_ass_bf

### Central Prime N400 Category Data ANOVA 
N400_central_super_all_cat$strength <- factor(N400_central_super_all_cat$strength)
N400_central_super_all_cat$VF <- factor(N400_central_super_all_cat$VF)
N400_central_super_all_cat$Pp <- factor(N400_central_super_all_cat$Pp)

N400_central_cat_ez <- ezANOVA(data = N400_central_super_all_cat, dv = All, wid = Pp, within = .(strength, VF))
apa.ezANOVA.table(
  N400_central_cat_ez,
  correction = "GG",
  table.title = "",
  table.number = NA
)

N4_cen_cat_bfMainEffects = lmBF(All ~ strength + VF, data =  N400_central_super_all_cat)
N4_cen_cat_bfInteraction = lmBF(All ~ strength + VF + strength:VF, data = N400_central_super_all_cat)
## Compare the two models
N4_cen_cat_bf = N4_cen_cat_bfInteraction / N4_cen_cat_bfMainEffects
N4_cen_cat_bf

# N400 Plots by Type and VF all
N400c_grouped_plot_all <- ggplot(N400c_grouped_sum[N400c_grouped_sum$correctness == "All",], aes(x=strength, y=mean_amp, fill = strength)) + facet_grid(VF ~ type) + geom_bar(stat="identity") + geom_errorbar(aes(ymin=mean_amp+se, ymax=mean_amp-se), width=.1) + xlab("") + ylab("Mean Amplitude") + ggtitle("N400 Central Prime Mean Amplitudes by VF and Type") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + theme(legend.title = element_blank()) + guides(fill=FALSE)
N400c_grouped_plot_all


# Central Prime, LPC Mean Amps (600-900ms) 
# Central: LPC Visualize data
#  LPC Central Visualization
LPCc_bin_sum <- summarySE(LPC_central, measurevar = "mean_amp", groupvars = c("bin_label", "correctness"))
LPCc_bin_sum

LPCc_grouped_sum <- summarySE(LPC_central, measurevar = "mean_amp", groupvars = c("VF", "type", "strength", "correctness"))
LPCc_grouped_sum


### Compare LPC mean amplitudes by strength of relatedness (strong, weak, unrelated) of all trials and correct only trials
# LPC mean amp tables and bin label plots
LPCc_correct <- summarySE(LPC_central, measurevar = "mean_amp", groupvars = c("correctness", "strength"))
LPCc_correct

LPCc_binplot_all <- ggplot(LPCc_bin_sum[LPCc_bin_sum$correctness == "All",], aes(x=bin_label, y=mean_amp, fill = bin_label)) + geom_bar(stat="identity") + geom_errorbar(aes(ymin=mean_amp+se, ymax=mean_amp-se), width=.1) + xlab("") + ylab("Mean Amplitude") + ggtitle("LPC Central Prime Mean Amplitudes") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + theme(legend.title = element_blank()) + guides(fill=FALSE)
LPCc_binplot_all

LPCc_binplot_correct <- ggplot(LPCc_bin_sum[LPCc_bin_sum$correctness == "Correct",], aes(x=bin_label, y=mean_amp, fill = bin_label)) + geom_bar(stat="identity") + geom_errorbar(aes(ymin=mean_amp+se, ymax=mean_amp-se), width=.1) + xlab("") + ylab("Mean Amplitude") + ggtitle("LPC Central Prime Mean Amplitudes, Before and After Cue Correct Trials Only") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + theme(legend.title = element_blank()) + guides(fill=FALSE)
LPCc_binplot_correct


# LPC: Make data with a superelectrode
LPC_central_super_all <- dcast(LPC_central[LPC_central$correctness == "All",], Pp + bin + bin_label + VF + type + strength ~ correctness, value.var="mean_amp", fun.aggregate = mean)
summarySE(LPC_central_super_all, measurevar = "All", groupvars = c("VF", "type"))


# LPC: Make data by type
LPC_central_super_all_ass <- LPC_central_super_all[LPC_central_super_all$type == "ASS",]
LPC_central_super_all_cat <- LPC_central_super_all[LPC_central_super_all$type == "CAT",]

#Check var assumptions
ggplot(LPC_central_super_all_ass, aes(x=All)) + geom_histogram()
ggplot(LPC_central_super_all_cat, aes(x=All)) + geom_histogram()


### Central Prime LPC Association Data ANOVA
# Central:LPC ANOVAs
#  LPC Central ANOVA for all trials 
LPC_central_super_all_ass$strength <- factor(LPC_central_super_all_ass$strength)
LPC_central_super_all_ass$VF <- factor(LPC_central_super_all_ass$VF)
LPC_central_super_all_ass$Pp <- factor(LPC_central_super_all_ass$Pp)

LPC_central_ass_ez <- ezANOVA(data = LPC_central_super_all_ass, dv = All, wid = Pp, within = .(strength, VF))
apa.ezANOVA.table(
  LPC_central_ass_ez,
  correction = "GG",
  table.title = "",
  table.number = NA
)

LPC_cen_ass_bfMainEffects = lmBF(All ~ strength + VF, data =  LPC_central_super_all_ass)
LPC_cen_ass_bfInteraction = lmBF(All ~ strength + VF + strength:VF, data = LPC_central_super_all_ass)
## Compare the two models
LPC_cen_ass_bf = LPC_cen_ass_bfInteraction / LPC_cen_ass_bfMainEffects
LPC_cen_ass_bf


### Central Prime LPC Category Data ANOVA
# Central:LPC ANOVAs for CAT
LPC_central_super_all_cat$strength <- factor(LPC_central_super_all_cat$strength)
LPC_central_super_all_cat$VF <- factor(LPC_central_super_all_cat$VF)
LPC_central_super_all_cat$Pp <- factor(LPC_central_super_all_cat$Pp)

LPC_central_cat_ez <- ezANOVA(data = LPC_central_super_all_cat, dv = All, wid = Pp, within = .(strength, VF))
apa.ezANOVA.table(
  LPC_central_cat_ez,
  correction = "GG",
  table.title = "",
  table.number = NA
)
LPC_cen_cat_bfMainEffects = lmBF(All ~ strength + VF, data =  LPC_central_super_all_cat)
LPC_cen_cat_bfInteraction = lmBF(All ~ strength + VF + strength:VF, data = LPC_central_super_all_cat)
## Compare the two models
LPC_cen_cat_bf = LPC_cen_cat_bfInteraction / LPC_cen_cat_bfMainEffects
LPC_cen_cat_bf

# LPC plots by type and VF all
LPCc_grouped_plot_all <- ggplot(LPCc_grouped_sum[LPCc_grouped_sum$correctness == "All",], aes(x=strength, y=mean_amp, fill = strength)) + facet_grid(VF ~ type) + geom_bar(stat="identity") + geom_errorbar(aes(ymin=mean_amp+se, ymax=mean_amp-se), width=.1) + xlab("") + ylab("Mean Amplitude") + ggtitle("LPC Central Prime Mean Amplitudes by VF and Type") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + theme(legend.title = element_blank()) + guides(fill=FALSE)
LPCc_grouped_plot_all

