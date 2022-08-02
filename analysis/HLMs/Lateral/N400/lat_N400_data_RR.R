library(reshape2)

# modify to local path 
data_path <- '~/OSF/data/Med300-500_SINGLETRIAL_data_asscat_lat.csv'

#  Read in latprime Data
N400_lat <- read.csv(data_path)
head(N400_lat)
colnames(N400_lat)


# N400 lat data
# remove the trials with amp of 0 (artifacts)
N400_lat_wide_st <- N400_lat[N400_lat$'X12' != 0,]

# create averaged amp column
N400_lat_wide_st$averaged_amp <- (N400_lat_wide_st$`X12` + N400_lat_wide_st$`X13` + N400_lat_wide_st$`X16` + N400_lat_wide_st$`X17` + N400_lat_wide_st$`X20` + N400_lat_wide_st$`X21` + N400_lat_wide_st$`X24` + N400_lat_wide_st$`X25`)/8

# grab the rows that are relevant
N400_lat_wide_st <- N400_lat_wide_st[N400_lat_wide_st$target_condition == "RVF-ASS-STRONG" | N400_lat_wide_st$target_condition == "LVF-ASS-STRONG" | N400_lat_wide_st$target_condition == "RVF-ASS-WEAK" | N400_lat_wide_st$target_condition == "LVF-ASS-WEAK" | N400_lat_wide_st$target_condition == "RVF-ASS-UNREL" | N400_lat_wide_st$target_condition == "LVF-ASS-UNREL" | N400_lat_wide_st$target_condition == "RVF-CAT-STRONG" | N400_lat_wide_st$target_condition == "LVF-CAT-STRONG" | N400_lat_wide_st$target_condition == "RVF-CAT-WEAK" | N400_lat_wide_st$target_condition == "LVF-CAT-WEAK" | N400_lat_wide_st$target_condition == "RVF-CAT-UNREL" | N400_lat_wide_st$target_condition == "LVF-CAT-UNREL",]

# make a column for VF
N400_lat_wide_st$VF <- ifelse(N400_lat_wide_st$target_condition == "RVF-ASS-STRONG" | N400_lat_wide_st$target_condition == "RVF-CAT-STRONG" | N400_lat_wide_st$target_condition == "RVF-ASS-WEAK" | N400_lat_wide_st$target_condition == "RVF-CAT-WEAK" | N400_lat_wide_st$target_condition == "RVF-ASS-UNREL" | N400_lat_wide_st$target_condition == "RVF-CAT-UNREL", "RVF", "LVF")

# make a column for strength
N400_lat_wide_st$strength <- ifelse(N400_lat_wide_st$target_condition == "RVF-ASS-STRONG" | N400_lat_wide_st$target_condition == "RVF-CAT-STRONG" | N400_lat_wide_st$target_condition == "LVF-ASS-STRONG" | N400_lat_wide_st$target_condition == "LVF-CAT-STRONG", "Strong", ifelse(N400_lat_wide_st$target_condition == "RVF-ASS-WEAK" | N400_lat_wide_st$target_condition == "RVF-CAT-WEAK" | N400_lat_wide_st$target_condition == "LVF-ASS-WEAK" | N400_lat_wide_st$target_condition == "LVF-CAT-WEAK", "Weak", ifelse(N400_lat_wide_st$target_condition == "RVF-ASS-UNREL" | N400_lat_wide_st$target_condition == "RVF-CAT-UNREL" | N400_lat_wide_st$target_condition == "LVF-ASS-UNREL" | N400_lat_wide_st$target_condition == "LVF-CAT-UNREL", "Unrel", "Uh-Oh")))

# make a column for type
N400_lat_wide_st$type <- ifelse(N400_lat_wide_st$target_condition == "RVF-ASS-STRONG" | N400_lat_wide_st$target_condition == "LVF-ASS-STRONG" | N400_lat_wide_st$target_condition == "RVF-ASS-WEAK" | N400_lat_wide_st$target_condition == "LVF-ASS-WEAK" | N400_lat_wide_st$target_condition == "RVF-ASS-UNREL" | N400_lat_wide_st$target_condition == "LVF-ASS-UNREL", "ASS", "CAT")

# make things factors
N400_lat_wide_st$pp_num <- as.factor(N400_lat_wide_st$participant)
N400_lat_wide_st$target_id <- as.factor(N400_lat_wide_st$target_word_id)
N400_lat_wide_st$VF <- as.factor(N400_lat_wide_st$VF)
N400_lat_wide_st$type <- as.factor(N400_lat_wide_st$type)
N400_lat_wide_st$strength <- as.factor(N400_lat_wide_st$strength)
N400_lat_wide_st$bin_num <- as.factor(N400_lat_wide_st$bin_num)
N400_lat_wide <- dcast(N400_lat_wide_st, pp_num ~target_condition, value.var = "averaged_amp", fun.aggregate = mean)

### Check number of trials per bin (make sure data formatting worked)
# Looks ok!

# Check trial numbers in each bin
with(N400_lat_wide_st, table(VF, type, strength))
