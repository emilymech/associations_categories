
"
Notes:
N400
electrodes: 13:RMFr, 12:LMFr, 16:LMCe, 17:RMCe, 20:MiCe, 21:MiPa, 24:LDPa, 25:RDPa
time window: 300-500

LPC
electrodes: 28:LMOc, 29:RMOc, 30:MiOc, 16:LMCe, 17:RMCe, 20:MiCe, 21:MiPa, 24:LDPa, 25:RDPa
time window: 600-900

Correct is based loosely on whether their judgements matched our intuitions
"

library(ez)
library(Rmisc)
library(ggplot2)
library(reshape2)
library(psych)
library(apaTables)
library(BayesFactor)

# modify to local data path
N4_path <- "~/OSF/data/Med300-500_lat.dat"
LPC_path <- "~/OSF/data/Med600-900_lat.dat"

#  Latprime Data
N400_lat <- read.table(N4_path, header = F)

LPC_lat <- read.table(LPC_path, header = F)


# Lateral N400: Add Headers, Rename, Reorder Columns, include=FALSE}
#  .dat files are 1 column without headers
#  Good example of how the .dat files will be ordered: http://kutaslab.ucsd.edu/erpmanpages/merp_manual.7.html
#  e.g., bin 1 channel 1 for Pp1, bin 1 channel 1 for Pp2...
#  e.g., bin 1 chan 2 for Pp1, bin 1 chan 2 for Pp2...
#  e.g., bin 2 chan 1 for Pp1, bin 2 chan 1 for Pp2...
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#  N400 lat:
describe(N400_lat) # gives the number of rows to do the rep math

#  rename V1 to something more descriptive
colnames(N400_lat)[colnames(N400_lat)=="V1"] <- "mean_amp"

#  add a column for Pp that repeats the Pp # (from .mcf) for the whole dataset
N400_lat$Pp <-     rep(c("1", "2", "3", "4", "5", "6","7",
                         "8","9","10","11", "12","13","14", 
                         "15" , "16", "17", "18", "19","20", "21", 
                         "22", "23", "24"), times = 192)


#  add a column for chan # 
N400_lat$chan <- rep(c("12", "13", "16", "17", "20", "21", "24", "25"), each = 24, times = 24)

#  add a column for chan label
N400_lat$chan_label <- ifelse(N400_lat$chan == "12", "LMFr",
                              ifelse(N400_lat$chan == "13", "RMFr",
                                     ifelse(N400_lat$chan == "16", "LMCe", 
                                            ifelse(N400_lat$chan == "17", "RMCe", 
                                                   ifelse(N400_lat$chan == "20", "MiCe", 
                                                          ifelse(N400_lat$chan == "21", "MiPa", 
                                                                 ifelse(N400_lat$chan == "24", "LDPa", 
                                                                        ifelse(N400_lat$chan == "25", "RDPa", 
                                                                               "Houston, you have a problem"))))))))


#  add a column for bin #
N400_lat$bin <- rep(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
                      22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33), each = 192)

# add a column for bin label
#  The correct trials are trials in which the behavioral response was correct either before the prompt or after the prompt, but
#  the bin labels have been abbreviated for convenience
N400_lat$bin_label <- ifelse(N400_lat$bin == 1, "RVF-Ass-Strong", 
                             ifelse(N400_lat$bin == 2, "LVF-Ass-Strong", 
                                    ifelse(N400_lat$bin == 3, "RVF-Ass-Weak", 
                                           ifelse(N400_lat$bin == 4, "LVF-Ass-Weak", 
                                                  ifelse(N400_lat$bin == 5, "RVF-Ass-Unrel", 
                                                         ifelse(N400_lat$bin == 6, "LVF-Ass-Unrel", 
                                                                ifelse(N400_lat$bin == 7, "RVF-Cat-Strong", 
                                                                       ifelse(N400_lat$bin == 8, "LVF-Cat-Strong", 
                                                                              ifelse(N400_lat$bin == 9, "RVF-Cat-Weak", 
                                                                                     ifelse(N400_lat$bin == 10, "LVF-Cat-Weak", 
                                                                                            ifelse(N400_lat$bin == 11, "RVF-Cat-Unrel", 
                                                                                                   ifelse(N400_lat$bin == 12, "LVF-Cat-Unrel",
                                                                                                          ifelse(N400_lat$bin == 22, "Correct RVF-Ass-Strong", 
                                                                                                                 ifelse(N400_lat$bin == 23, "Correct LVF-Ass-Strong", 
                                                                                                                        ifelse(N400_lat$bin == 24, "Correct RVF-Ass-Weak", 
                                                                                                                               ifelse(N400_lat$bin == 25, "Correct LVF-Ass-Weak", 
                                                                                                                                      ifelse(N400_lat$bin == 26, "Correct RVF-Ass-Unrel", 
                                                                                                                                             ifelse(N400_lat$bin == 27, "Correct LVF-Ass-Unrel", 
                                                                                                                                                    ifelse(N400_lat$bin == 28, "Correct RVF-Cat-Strong", 
                                                                                                                                                           ifelse(N400_lat$bin == 29, "Correct LVF-Cat-Strong", 
                                                                                                                                                                  ifelse(N400_lat$bin == 30, "Correct RVF-Cat-Weak",
                                                                                                                                                                         ifelse(N400_lat$bin == 31, "Correct LVF-Cat-Weak", 
                                                                                                                                                                                ifelse(N400_lat$bin == 32, "Correct RVF-Cat-Unrel", 
                                                                                                                                                                                       ifelse(N400_lat$bin == 33, "Correct LVF-Cat-Unrel", 
                                                                                                                                                                                              "Houston, you have a problem"))))))))))))))))))))))))

#  add a column to group RVF and LVF
N400_lat$VF <- ifelse(N400_lat$bin == 1 | 
                        N400_lat$bin == 3 |
                        N400_lat$bin == 5 |
                        N400_lat$bin == 7 |
                        N400_lat$bin == 9 | 
                        N400_lat$bin == 11 |
                        N400_lat$bin == 22 | 
                        N400_lat$bin == 24 |
                        N400_lat$bin == 26 |
                        N400_lat$bin == 28 |
                        N400_lat$bin == 30 | 
                        N400_lat$bin == 32, 
                      "RVF", "LVF")
#  add a column to group by type of relatedness
N400_lat$type <- ifelse(N400_lat$bin == 1 | 
                          N400_lat$bin == 2 |
                          N400_lat$bin == 3 |
                          N400_lat$bin == 4 |
                          N400_lat$bin == 5 | 
                          N400_lat$bin == 6 | 
                          N400_lat$bin == 22 | 
                          N400_lat$bin == 23 |
                          N400_lat$bin == 24 |
                          N400_lat$bin == 25 |
                          N400_lat$bin == 26 | 
                          N400_lat$bin == 27, 
                        "ASS", "CAT")

#  add a column to group by strength of stimulus
N400_lat$strength <- ifelse(N400_lat$bin == 1 | 
                              N400_lat$bin == 2 |
                              N400_lat$bin == 7 |
                              N400_lat$bin == 8 |
                              N400_lat$bin == 22 | 
                              N400_lat$bin == 23 |
                              N400_lat$bin == 28 |
                              N400_lat$bin == 29,
                            "Strong", 
                            ifelse(N400_lat$bin == 3|
                                     N400_lat$bin == 4|
                                     N400_lat$bin == 9|
                                     N400_lat$bin == 10|
                                     N400_lat$bin == 24|
                                     N400_lat$bin == 25|
                                     N400_lat$bin == 30|
                                     N400_lat$bin == 31, 
                                   "Weak", "Unrelated"))

#  add a column to group by whether the behavioral response was correct or incorrect
N400_lat$correctness <- ifelse(N400_lat$bin >21, "Correct", "All")

#  reorder column names for personal sanity
N400_lat <- N400_lat[c("Pp", "bin", "bin_label", "VF", "type", "strength","correctness", "chan", "chan_label", "mean_amp")]

#  double check that the data look ok
#  View(N400_lat)
#  head(N400_lat)
#  colnames(N400_lat)


# Lateral LPC: Add Headers, Rename, Reorder Columns
#  LPC lat:
describe(LPC_lat) # gives the number of rows to do the rep math

#  rename V1 to something more descriptive
colnames(LPC_lat)[colnames(LPC_lat)=="V1"] <- "mean_amp"

#  add a column for Pp that repeats the Pp # (from .mcf) for the whole dataset
LPC_lat$Pp <-      rep(c("1", "2", "3", "4", "5", "6","7",
                         "8","9","10","11", "12","13","14", 
                         "15" , "16", "17", "18", "19","20", "21", 
                         "22", "23", "24"), times = 216)


#  add a column for chan # 
LPC_lat$chan <- rep(c("16", "17", "20", "21", "24", "25", "28", "29", "30"), each = 24, times = 24)

#  add a column for chan label
LPC_lat$chan_label <- ifelse(LPC_lat$chan == "16", "LMCe", 
                             ifelse(LPC_lat$chan == "17", "RMCe", 
                                    ifelse(LPC_lat$chan == "20", "MiCe", 
                                           ifelse(LPC_lat$chan == "21", "MiPa", 
                                                  ifelse(LPC_lat$chan == "24", "LDPa", 
                                                         ifelse(LPC_lat$chan == "25", "RDPa", 
                                                                ifelse(LPC_lat$chan == "28", "LMOc", 
                                                                       ifelse(LPC_lat$chan == "29", "RMOc", 
                                                                              ifelse(LPC_lat$chan == "30", "MiOc",
                                                                                     "Houston, you have a problem")))))))))


#  add a column for bin #
LPC_lat$bin <- rep(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12,
                     22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33), each = 216)

# add a column for bin label
#  The correct trials are trials in which the behavioral response was correct either before the prompt or after the prompt, but
#  the bin labels have been abbreviated for convenience
LPC_lat$bin_label <- ifelse(LPC_lat$bin == 1, "RVF-Ass-Strong", 
                            ifelse(LPC_lat$bin == 2, "LVF-Ass-Strong", 
                                   ifelse(LPC_lat$bin == 3, "RVF-Ass-Weak", 
                                          ifelse(LPC_lat$bin == 4, "LVF-Ass-Weak", 
                                                 ifelse(LPC_lat$bin == 5, "RVF-Ass-Unrel", 
                                                        ifelse(LPC_lat$bin == 6, "LVF-Ass-Unrel", 
                                                               ifelse(LPC_lat$bin == 7, "RVF-Cat-Strong", 
                                                                      ifelse(LPC_lat$bin == 8, "LVF-Cat-Strong", 
                                                                             ifelse(LPC_lat$bin == 9, "RVF-Cat-Weak", 
                                                                                    ifelse(LPC_lat$bin == 10, "LVF-Cat-Weak", 
                                                                                           ifelse(LPC_lat$bin == 11, "RVF-Cat-Unrel", 
                                                                                                  ifelse(LPC_lat$bin == 12, "LVF-Cat-Unrel",
                                                                                                         ifelse(LPC_lat$bin == 22, "Correct RVF-Ass-Strong", 
                                                                                                                ifelse(LPC_lat$bin == 23, "Correct LVF-Ass-Strong", 
                                                                                                                       ifelse(LPC_lat$bin == 24, "Correct RVF-Ass-Weak", 
                                                                                                                              ifelse(LPC_lat$bin == 25, "Correct LVF-Ass-Weak", 
                                                                                                                                     ifelse(LPC_lat$bin == 26, "Correct RVF-Ass-Unrel", 
                                                                                                                                            ifelse(LPC_lat$bin == 27, "Correct LVF-Ass-Unrel", 
                                                                                                                                                   ifelse(LPC_lat$bin == 28, "Correct RVF-Cat-Strong", 
                                                                                                                                                          ifelse(LPC_lat$bin == 29, "Correct LVF-Cat-Strong", 
                                                                                                                                                                 ifelse(LPC_lat$bin == 30, "Correct RVF-Cat-Weak",
                                                                                                                                                                        ifelse(LPC_lat$bin == 31, "Correct LVF-Cat-Weak", 
                                                                                                                                                                               ifelse(LPC_lat$bin == 32, "Correct RVF-Cat-Unrel", 
                                                                                                                                                                                      ifelse(LPC_lat$bin == 33, "Correct LVF-Cat-Unrel", 
                                                                                                                                                                                             "Houston, you have a problem"))))))))))))))))))))))))

#  add a column to group RVF and LVF
LPC_lat$VF <- ifelse(LPC_lat$bin == 1 | 
                       LPC_lat$bin == 3 |
                       LPC_lat$bin == 5 |
                       LPC_lat$bin == 7 |
                       LPC_lat$bin == 9 | 
                       LPC_lat$bin == 11 |
                       LPC_lat$bin == 22 | 
                       LPC_lat$bin == 24 |
                       LPC_lat$bin == 26 |
                       LPC_lat$bin == 28 |
                       LPC_lat$bin == 30 | 
                       LPC_lat$bin == 32, 
                     "RVF", "LVF")
#  add a column to group by type of relatedness
LPC_lat$type <- ifelse(LPC_lat$bin == 1 | 
                         LPC_lat$bin == 2 |
                         LPC_lat$bin == 3 |
                         LPC_lat$bin == 4 |
                         LPC_lat$bin == 5 | 
                         LPC_lat$bin == 6 | 
                         LPC_lat$bin == 22 | 
                         LPC_lat$bin == 23 |
                         LPC_lat$bin == 24 |
                         LPC_lat$bin == 25 |
                         LPC_lat$bin == 26 | 
                         LPC_lat$bin == 27, 
                       "ASS", "CAT")

#  add a column to group by strength of stimulus
LPC_lat$strength <- ifelse(LPC_lat$bin == 1 | 
                             LPC_lat$bin == 2 |
                             LPC_lat$bin == 7 |
                             LPC_lat$bin == 8 |
                             LPC_lat$bin == 22 | 
                             LPC_lat$bin == 23 |
                             LPC_lat$bin == 28 |
                             LPC_lat$bin == 29,
                           "Strong", 
                           ifelse(LPC_lat$bin == 3|
                                    LPC_lat$bin == 4|
                                    LPC_lat$bin == 9|
                                    LPC_lat$bin == 10|
                                    LPC_lat$bin == 24|
                                    LPC_lat$bin == 25|
                                    LPC_lat$bin == 30|
                                    LPC_lat$bin == 31, 
                                  "Weak", "Unrelated"))

#  add a column to group by whether the behavioral response was correct or incorrect
LPC_lat$correctness <- ifelse(LPC_lat$bin >21, "Correct", "All")

#  reorder column names for personal sanity
LPC_lat <- LPC_lat[c("Pp", "bin", "bin_label", "VF", "type", "strength", "correctness", "chan", "chan_label", "mean_amp")]

#  double check that the data look ok
#  View(LPC_lat)
#  head(LPC_lat)
#  colnames(LPC_lat)

# Lateral Prime, N400 Mean Amps (300-500ms) 
# lat: N400 Visualize Data
#  N400 lat Visualization
N400l_bin_sum <- summarySE(N400_lat, measurevar = "mean_amp", groupvars = c("bin_label", "correctness"))
N400l_bin_sum

N400l_grouped_sum <- summarySE(N400_lat, measurevar = "mean_amp", groupvars = c("VF", "type", "strength", "correctness"))
N400l_grouped_sum


### Compare N400 mean amplitudes by strength of relatedness (strong, weak, unrelated) of all trials and correct only trials
# N400 mean amp table and plots by bin label
N400l_correct <- summarySE(N400_lat, measurevar = "mean_amp", groupvars = c("correctness","strength"))
N400l_correct

N400l_binplot_all <- ggplot(N400l_bin_sum[N400l_bin_sum$correctness == "All",], aes(x=bin_label, y=mean_amp, fill = bin_label)) + geom_bar(stat="identity") + geom_errorbar(aes(ymin=mean_amp+se, ymax=mean_amp-se), width=.1) + xlab("") + ylab("Mean Amplitude") + ggtitle("N400 Lateral Prime Mean Amplitudes") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + theme(legend.title = element_blank()) + guides(fill=FALSE)
N400l_binplot_all

N400l_binplot_correct <- ggplot(N400l_bin_sum[N400l_bin_sum$correctness == "Correct",], aes(x=bin_label, y=mean_amp, fill = bin_label)) + geom_bar(stat="identity") + geom_errorbar(aes(ymin=mean_amp+se, ymax=mean_amp-se), width=.1) + xlab("") + ylab("Mean Amplitude") + ggtitle("N400 Lateral Prime Mean Amplitudes, Before and After Cue Correct Trials Only") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + theme(legend.title = element_blank()) + guides(fill=FALSE)
N400l_binplot_correct


# Make data with a superelectrode
N400_lat_super_all <- dcast(N400_lat[N400_lat$correctness == "All",], Pp + bin + bin_label + VF + type + strength ~ correctness, value.var="mean_amp", fun.aggregate = mean)
summarySE(N400_lat_super_all, measurevar = "All", groupvars = c("VF", "type"))

LPC_lat_super_all <- dcast(LPC_lat[LPC_lat$correctness == "All",], Pp + bin + bin_label + VF + type + strength ~ correctness, value.var="mean_amp", fun.aggregate = mean)
summarySE(LPC_lat_super_all, measurevar = "All", groupvars = c("VF", "type"))

# Make data by Type
N400_lat_super_all_ass <- N400_lat_super_all[N400_lat_super_all$type == "ASS",]
N400_lat_super_all_cat <- N400_lat_super_all[N400_lat_super_all$type == "CAT",]

#Check var assumptions
ggplot(N400_lat_super_all_ass, aes(x=All)) + geom_histogram()
ggplot(N400_lat_super_all_ass, aes(x=All)) + geom_histogram()


## LPC
LPC_lat_super_all_ass <- LPC_lat_super_all[LPC_lat_super_all$type == "ASS",]
LPC_lat_super_all_cat <- LPC_lat_super_all[LPC_lat_super_all$type == "CAT",]

#Check var assumptions
ggplot(LPC_lat_super_all_ass, aes(x=All)) + geom_histogram()
ggplot(LPC_lat_super_all_ass, aes(x=All)) + geom_histogram()

### Lateral Prime N400 Association Data ANOVA
# Lateral: N400 ASS ANOVAs
N400_lat_super_all_ass$Pp <- as.factor(N400_lat_super_all_ass$Pp)
N400_lat_super_all_ass$strength <- as.factor(N400_lat_super_all_ass$strength)
N400_lat_super_all_ass$VF <- as.factor(N400_lat_super_all_ass$VF)

N400_lat_ass_ez <- ezANOVA(data = N400_lat_super_all_ass, dv = All, wid = Pp, within = .(strength, VF))
apa.ezANOVA.table(
  N400_lat_ass_ez,
  correction = "GG",
  table.title = "",
  table.number = NA
)
N400_lat_ass_bfMainEffects = lmBF(All ~ strength + VF, data =  N400_lat_super_all_ass)
N400_lat_ass_bfInteraction = lmBF(All ~ strength + VF + strength:VF, data = N400_lat_super_all_ass)
## Compare the two models
N400_lat_ass_bf = N400_lat_ass_bfInteraction / N400_lat_ass_bfMainEffects
N400_lat_ass_bf


### Lateral Prime N400 Category Data ANOVA
# N400 CAT ANOVAs
N400_lat_super_all_cat$Pp <- as.factor(N400_lat_super_all_cat$Pp)
N400_lat_super_all_cat$strength <- as.factor(N400_lat_super_all_cat$strength)
N400_lat_super_all_cat$VF <- as.factor(N400_lat_super_all_cat$VF)

N400_lat_cat_ez <- ezANOVA(data = N400_lat_super_all_cat, dv = All, wid = Pp, within = .(strength, VF))
apa.ezANOVA.table(
  N400_lat_cat_ez,
  correction = "GG",
  table.title = "",
  table.number = NA
)

N400_lat_cat_bfMainEffects = lmBF(All ~ strength + VF, data =  N400_lat_super_all_cat)
N400_lat_cat_bfInteraction = lmBF(All ~ strength + VF + strength:VF, data = N400_lat_super_all_cat)
## Compare the two models
N400_lat_cat_bf = N400_lat_cat_bfInteraction / N400_lat_cat_bfMainEffects
N400_lat_cat_bf

# N400 Plots by Type and VF all
N400l_grouped_plot_all <- ggplot(N400l_grouped_sum[N400l_grouped_sum$correctness == "All",], aes(x=strength, y=mean_amp, fill = strength)) + facet_grid(VF ~ type) + geom_bar(stat="identity") + geom_errorbar(aes(ymin=mean_amp+se, ymax=mean_amp-se), width=.1) + xlab("") + ylab("Mean Amplitude") + ggtitle("N400 Lateral Prime Mean Amplitudes by VF and Type") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + theme(legend.title = element_blank()) + guides(fill=FALSE)
N400l_grouped_plot_all


# Lateral Prime, LPC Mean Amps (600-900ms)
# lat: LPC Visualize Data
#  LPC lat Visualization
LPCl_bin_sum <- summarySE(LPC_lat, measurevar = "mean_amp", groupvars = c("bin_label", "correctness"))
LPCl_bin_sum

LPCl_grouped_sum <- summarySE(LPC_lat, measurevar = "mean_amp", groupvars = c("VF", "type", "strength", "correctness"))
LPCl_grouped_sum

### Compare LPC mean amplitudes by strength of relatedness (strong, weak, unrelated) of all trials and correct only trials
# LPC mean amp tables and bin label plots
LPCl_correct <- summarySE(LPC_lat, measurevar = "mean_amp", groupvars = c("correctness", "strength"))
LPCl_correct

LPCl_binplot_all <- ggplot(LPCl_bin_sum[LPCl_bin_sum$correctness == "All",], aes(x=bin_label, y=mean_amp, fill = bin_label)) + geom_bar(stat="identity") + geom_errorbar(aes(ymin=mean_amp+se, ymax=mean_amp-se), width=.1) + xlab("") + ylab("Mean Amplitude") + ggtitle("LPC Lateral Prime Mean Amplitudes") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + theme(legend.title = element_blank()) + guides(fill=FALSE)
LPCl_binplot_all

LPCl_binplot_correct <- ggplot(LPCl_bin_sum[LPCl_bin_sum$correctness == "Correct",], aes(x=bin_label, y=mean_amp, fill = bin_label)) + geom_bar(stat="identity") + geom_errorbar(aes(ymin=mean_amp+se, ymax=mean_amp-se), width=.1) + xlab("") + ylab("Mean Amplitude") + ggtitle("LPC Lateral Prime Mean Amplitudes, Before and After Cue Correct Trials Only") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + theme(legend.title = element_blank()) + guides(fill=FALSE)
LPCl_binplot_correct

### Lateral Prime LPC Association Data ANOVA and follow-up t-tests for all trials
# LPC ANOVAs for ASS and All trials
#  LPC lat ANOVA for all trials (ass then cat)
LPC_lat_super_all_ass$Pp <- as.factor(LPC_lat_super_all_ass$Pp)
LPC_lat_super_all_ass$strength <- as.factor(LPC_lat_super_all_ass$strength)
LPC_lat_super_all_ass$VF <- as.factor(LPC_lat_super_all_ass$VF)
LPC_lat_ass_ez <-ezANOVA(data = LPC_lat_super_all_ass, dv = All, wid = Pp, within = .(strength, VF))
apa.ezANOVA.table(
  LPC_lat_ass_ez,
  correction = "GG",
  table.title = "",
  table.number = NA
)

LPC_lat_ass_bfMainEffects = lmBF(All ~ strength + VF, data =  LPC_lat_super_all_ass)
LPC_lat_ass_bfInteraction = lmBF(All ~ strength + VF + strength:VF, data = LPC_lat_super_all_ass)
## Compare the two models
LPC_lat_ass_bf = LPC_lat_ass_bfInteraction / LPC_lat_ass_bfMainEffects
LPC_lat_ass_bf


### Lateral Prime LPC Category Data ANOVA
# LPC ANOVAs for CAT
LPC_lat_super_all_cat$Pp <- as.factor(LPC_lat_super_all_cat$Pp)
LPC_lat_super_all_cat$strength <- as.factor(LPC_lat_super_all_cat$strength)
LPC_lat_super_all_cat$VF <- as.factor(LPC_lat_super_all_cat$VF)
LPC_lat_cat_ez <- ezANOVA(data = LPC_lat_super_all_cat, dv = All, wid = Pp, within = .(strength, VF))
apa.ezANOVA.table(
  LPC_lat_cat_ez,
  correction = "GG",
  table.title = "",
  table.number = NA
)
LPC_lat_cat_bfMainEffects = lmBF(All ~ strength + VF, data =  LPC_lat_super_all_cat)
LPC_lat_cat_bfInteraction = lmBF(All ~ strength + VF + strength:VF, data = LPC_lat_super_all_cat)
## Compare the two models
LPC_lat_cat_bf = LPC_lat_cat_bfInteraction / LPC_lat_cat_bfMainEffects
LPC_lat_cat_bf

# LPC plots by type and VF all
LPCl_grouped_plot_all <- ggplot(LPCl_grouped_sum[LPCl_grouped_sum$correctness == "All",], aes(x=strength, y=mean_amp, fill = strength)) + facet_grid(VF ~ type) + geom_bar(stat="identity") + geom_errorbar(aes(ymin=mean_amp+se, ymax=mean_amp-se), width=.1) + xlab("") + ylab("Mean Amplitude") + ggtitle("LPC Lateral Prime Mean Amplitudes by VF and Type") + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + theme(legend.title = element_blank()) + guides(fill=FALSE)
LPCl_grouped_plot_all

