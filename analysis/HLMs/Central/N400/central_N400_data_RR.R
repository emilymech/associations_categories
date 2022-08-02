library(reshape2)

data_path <- "~/OSF/data/Med300-500_SINGLETRIAL_data_asscat_central.csv"
  
#  Read in Centralprime Data
N400_central <- read.csv(data_path)
head(N400_central)
colnames(N400_central)


# N400 central data (get rid of artifacts)
N400_central_wide_st <- N400_central[N400_central$'X12' != 0,] # one electrode is sufficient because artifact trials have 0's everywhere

# add an averaged amp column
N400_central_wide_st$averaged_amp <- (N400_central_wide_st$`X12` + N400_central_wide_st$`X13` + N400_central_wide_st$`X16` + N400_central_wide_st$`X17` + N400_central_wide_st$`X20` + N400_central_wide_st$`X21` + N400_central_wide_st$`X24` + N400_central_wide_st$`X25`)/8

# make a VF column
N400_central_wide_st$VF <- ifelse(N400_central_wide_st$target_condition == "RVF-ASS-STRONG" | N400_central_wide_st$target_condition == "RVF-CAT-STRONG" | N400_central_wide_st$target_condition == "RVF-ASS-WEAK" | N400_central_wide_st$target_condition == "RVF-CAT-WEAK" | N400_central_wide_st$target_condition == "RVF-ASS-UNREL" | N400_central_wide_st$target_condition == "RVF-CAT-UNREL", "RVF", "LVF")

# make a strength column
N400_central_wide_st$strength <- ifelse(N400_central_wide_st$target_condition == "RVF-ASS-STRONG" | N400_central_wide_st$target_condition == "RVF-CAT-STRONG" | N400_central_wide_st$target_condition == "LVF-ASS-STRONG" | N400_central_wide_st$target_condition == "LVF-CAT-STRONG", "Strong", ifelse(N400_central_wide_st$target_condition == "RVF-ASS-WEAK" | N400_central_wide_st$target_condition == "RVF-CAT-WEAK" | N400_central_wide_st$target_condition == "LVF-ASS-WEAK" | N400_central_wide_st$target_condition == "LVF-CAT-WEAK", "Weak", ifelse(N400_central_wide_st$target_condition == "RVF-ASS-UNREL" | N400_central_wide_st$target_condition == "RVF-CAT-UNREL" | N400_central_wide_st$target_condition == "LVF-ASS-UNREL" | N400_central_wide_st$target_condition == "LVF-CAT-UNREL", "Unrel", "Uh-Oh")))

# make a type column
N400_central_wide_st$type <- ifelse(N400_central_wide_st$target_condition == "RVF-ASS-STRONG" | N400_central_wide_st$target_condition == "LVF-ASS-STRONG" | N400_central_wide_st$target_condition == "RVF-ASS-WEAK" | N400_central_wide_st$target_condition == "LVF-ASS-WEAK" | N400_central_wide_st$target_condition == "RVF-ASS-UNREL" | N400_central_wide_st$target_condition == "LVF-ASS-UNREL", "ASS", "CAT")

# make factors
N400_central_wide_st$pp_num <- as.factor(N400_central_wide_st$participant)
N400_central_wide_st$target_id <- as.factor(N400_central_wide_st$target_word_id)
N400_central_wide_st$VF <- as.factor(N400_central_wide_st$VF)
N400_central_wide_st$type <- as.factor(N400_central_wide_st$type)
N400_central_wide_st$strength <- as.factor(N400_central_wide_st$strength)
N400_central_wide_st$bin_num <- as.factor(N400_central_wide_st$bin_num)

# table with averaged values per condition per participant
N400_central_wide <- dcast(N400_central_wide_st, pp_num ~target_condition, value.var = "averaged_amp", fun.aggregate = mean)

### Check number of trials per bin (make sure data formatting worked)
# Looks ok!

# Check trial numbers in each bin
with(N400_central_wide_st, table(VF, type, strength))





