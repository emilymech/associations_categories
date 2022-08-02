library(reshape2)

# Load Data
# modify to local path
data_path <- '~/OSF/data/Med600-900_SINGLETRIAL_data_asscat_lat.csv'

#  Read in latprime Data
LPC_lat <- read.csv(data_path)
head(LPC_lat)
colnames(LPC_lat)


# LPC lat data
# remove rows with 0 amplitude (artifacts)
LPC_lat_wide_st <- LPC_lat[LPC_lat$'X16' != 0,]

# make a column for averaged amplitudes
LPC_lat_wide_st$averaged_amp <- (LPC_lat_wide_st$`X16` + LPC_lat_wide_st$`X17` + LPC_lat_wide_st$`X20` + LPC_lat_wide_st$`X21` + LPC_lat_wide_st$`X24` + LPC_lat_wide_st$`X25` + LPC_lat_wide_st$`X28` + LPC_lat_wide_st$`X29`+ LPC_lat_wide_st$`X30`)/9

# grab only the necessary rows
LPC_lat_wide_st <- LPC_lat_wide_st[LPC_lat_wide_st$target_condition == "RVF-ASS-STRONG" | LPC_lat_wide_st$target_condition == "LVF-ASS-STRONG" | LPC_lat_wide_st$target_condition == "RVF-ASS-WEAK" | LPC_lat_wide_st$target_condition == "LVF-ASS-WEAK" | LPC_lat_wide_st$target_condition == "RVF-ASS-UNREL" | LPC_lat_wide_st$target_condition == "LVF-ASS-UNREL" | LPC_lat_wide_st$target_condition == "RVF-CAT-STRONG" | LPC_lat_wide_st$target_condition == "LVF-CAT-STRONG" | LPC_lat_wide_st$target_condition == "RVF-CAT-WEAK" | LPC_lat_wide_st$target_condition == "LVF-CAT-WEAK" | LPC_lat_wide_st$target_condition == "RVF-CAT-UNREL" | LPC_lat_wide_st$target_condition == "LVF-CAT-UNREL",]

# make a column for VF
LPC_lat_wide_st$VF <- ifelse(LPC_lat_wide_st$target_condition == "RVF-ASS-STRONG" | LPC_lat_wide_st$target_condition == "RVF-CAT-STRONG" | LPC_lat_wide_st$target_condition == "RVF-ASS-WEAK" | LPC_lat_wide_st$target_condition == "RVF-CAT-WEAK" | LPC_lat_wide_st$target_condition == "RVF-ASS-UNREL" | LPC_lat_wide_st$target_condition == "RVF-CAT-UNREL", "RVF", "LVF")

# make a column for strength
LPC_lat_wide_st$strength <- ifelse(LPC_lat_wide_st$target_condition == "RVF-ASS-STRONG" | LPC_lat_wide_st$target_condition == "RVF-CAT-STRONG" | LPC_lat_wide_st$target_condition == "LVF-ASS-STRONG" | LPC_lat_wide_st$target_condition == "LVF-CAT-STRONG", "Strong", ifelse(LPC_lat_wide_st$target_condition == "RVF-ASS-WEAK" | LPC_lat_wide_st$target_condition == "RVF-CAT-WEAK" | LPC_lat_wide_st$target_condition == "LVF-ASS-WEAK" | LPC_lat_wide_st$target_condition == "LVF-CAT-WEAK", "Weak", ifelse(LPC_lat_wide_st$target_condition == "RVF-ASS-UNREL" | LPC_lat_wide_st$target_condition == "RVF-CAT-UNREL" | LPC_lat_wide_st$target_condition == "LVF-ASS-UNREL" | LPC_lat_wide_st$target_condition == "LVF-CAT-UNREL", "Unrel", "Uh-Oh")))

# make a column for type
LPC_lat_wide_st$type <- ifelse(LPC_lat_wide_st$target_condition == "RVF-ASS-STRONG" | LPC_lat_wide_st$target_condition == "LVF-ASS-STRONG" | LPC_lat_wide_st$target_condition == "RVF-ASS-WEAK" | LPC_lat_wide_st$target_condition == "LVF-ASS-WEAK" | LPC_lat_wide_st$target_condition == "RVF-ASS-UNREL" | LPC_lat_wide_st$target_condition == "LVF-ASS-UNREL", "ASS", "CAT")

# make vars into factors
LPC_lat_wide_st$pp_num <- as.factor(LPC_lat_wide_st$participant)
LPC_lat_wide_st$target_id <- as.factor(LPC_lat_wide_st$target_word_id)
LPC_lat_wide_st$VF <- as.factor(LPC_lat_wide_st$VF)
LPC_lat_wide_st$type <- as.factor(LPC_lat_wide_st$type)
LPC_lat_wide_st$strength <- as.factor(LPC_lat_wide_st$strength)
LPC_lat_wide_st$bin_num <- as.factor(LPC_lat_wide_st$bin_num)

# make data by condition and by participant
LPC_lat_wide <- dcast(LPC_lat_wide_st, pp_num ~target_condition, value.var = "averaged_amp", fun.aggregate = mean)

### Check number of trials per bin (make sure data formatting worked)
# Looks ok!

# Check trial numbers in each bin
with(LPC_lat_wide_st, table(VF, type, strength))
