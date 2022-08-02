
# Load Libraries, Functions, and Set Theme
library(reshape2)

# modify to local data path
LPC_central_path <- "~/OSF/data/Med600-900_SINGLETRIAL_data_asscat_central.csv"

#  Read in centralprime Data
LPC_central <- read.csv(LPC_central_path)
head(LPC_central)
colnames(LPC_central)


# LPC central data

# get rid of trials with 0 amp (artifact rejected trials)
LPC_central_wide_st <- LPC_central[LPC_central$'X16' != 0,]

# make averaged amp column
LPC_central_wide_st$averaged_amp <- (LPC_central_wide_st$`X16` + LPC_central_wide_st$`X17` + LPC_central_wide_st$`X20` + LPC_central_wide_st$`X21` + LPC_central_wide_st$`X24` + LPC_central_wide_st$`X25` + LPC_central_wide_st$`X28` + LPC_central_wide_st$`X29`+ LPC_central_wide_st$`X30`)/9

# get relevant rows
LPC_central_wide_st <- LPC_central_wide_st[LPC_central_wide_st$target_condition == "RVF-ASS-STRONG" | LPC_central_wide_st$target_condition == "LVF-ASS-STRONG" | LPC_central_wide_st$target_condition == "RVF-ASS-WEAK" | LPC_central_wide_st$target_condition == "LVF-ASS-WEAK" | LPC_central_wide_st$target_condition == "RVF-ASS-UNREL" | LPC_central_wide_st$target_condition == "LVF-ASS-UNREL" | LPC_central_wide_st$target_condition == "RVF-CAT-STRONG" | LPC_central_wide_st$target_condition == "LVF-CAT-STRONG" | LPC_central_wide_st$target_condition == "RVF-CAT-WEAK" | LPC_central_wide_st$target_condition == "LVF-CAT-WEAK" | LPC_central_wide_st$target_condition == "RVF-CAT-UNREL" | LPC_central_wide_st$target_condition == "LVF-CAT-UNREL",]

# make column for VF
LPC_central_wide_st$VF <- ifelse(LPC_central_wide_st$target_condition == "RVF-ASS-STRONG" | LPC_central_wide_st$target_condition == "RVF-CAT-STRONG" | LPC_central_wide_st$target_condition == "RVF-ASS-WEAK" | LPC_central_wide_st$target_condition == "RVF-CAT-WEAK" | LPC_central_wide_st$target_condition == "RVF-ASS-UNREL" | LPC_central_wide_st$target_condition == "RVF-CAT-UNREL", "RVF", "LVF")

# make column for strength
LPC_central_wide_st$strength <- ifelse(LPC_central_wide_st$target_condition == "RVF-ASS-STRONG" | LPC_central_wide_st$target_condition == "RVF-CAT-STRONG" | LPC_central_wide_st$target_condition == "LVF-ASS-STRONG" | LPC_central_wide_st$target_condition == "LVF-CAT-STRONG", "Strong", ifelse(LPC_central_wide_st$target_condition == "RVF-ASS-WEAK" | LPC_central_wide_st$target_condition == "RVF-CAT-WEAK" | LPC_central_wide_st$target_condition == "LVF-ASS-WEAK" | LPC_central_wide_st$target_condition == "LVF-CAT-WEAK", "Weak", ifelse(LPC_central_wide_st$target_condition == "RVF-ASS-UNREL" | LPC_central_wide_st$target_condition == "RVF-CAT-UNREL" | LPC_central_wide_st$target_condition == "LVF-ASS-UNREL" | LPC_central_wide_st$target_condition == "LVF-CAT-UNREL", "Unrel", "Uh-Oh")))

# make column for type
LPC_central_wide_st$type <- ifelse(LPC_central_wide_st$target_condition == "RVF-ASS-STRONG" | LPC_central_wide_st$target_condition == "LVF-ASS-STRONG" | LPC_central_wide_st$target_condition == "RVF-ASS-WEAK" | LPC_central_wide_st$target_condition == "LVF-ASS-WEAK" | LPC_central_wide_st$target_condition == "RVF-ASS-UNREL" | LPC_central_wide_st$target_condition == "LVF-ASS-UNREL", "ASS", "CAT")

# make factors
LPC_central_wide_st$pp_num <- as.factor(LPC_central_wide_st$participant)
LPC_central_wide_st$target_id <- as.factor(LPC_central_wide_st$target_word_id)
LPC_central_wide_st$VF <- as.factor(LPC_central_wide_st$VF)
LPC_central_wide_st$type <- as.factor(LPC_central_wide_st$type)
LPC_central_wide_st$strength <- as.factor(LPC_central_wide_st$strength)
LPC_central_wide_st$bin_num <- as.factor(LPC_central_wide_st$bin_num)

# make data by condition and participant
LPC_central_wide <- dcast(LPC_central_wide_st, pp_num ~target_condition, value.var = "averaged_amp", fun.aggregate = mean)

### Check number of trials per bin (make sure data formatting worked)
# Looks ok!

# Check trial numbers in each bin
with(LPC_central_wide_st, table(VF, type, strength))

