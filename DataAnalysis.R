library(dplyr)
library(ncvreg)

nypd <- read.csv("//storage/Projects/Mat/STA310-Kuiper/Final Project/4_LeeKozikJindal_NYPD/nypd_data.csv")
cols <- colnames(nypd)
nypd[cols] <- lapply(nypd[cols], factor) 
nypd$OBSERVED_DURATION_MINUTES <- as.numeric(nypd$OBSERVED_DURATION_MINUTES)
nypd$STOP_DURATION_MINUTES <- as.numeric(nypd$STOP_DURATION_MINUTES)
nypd$STOP_LOCATION_X <- as.numeric(nypd$STOP_LOCATION_X)
nypd$STOP_LOCATION_Y <- as.numeric(nypd$STOP_LOCATION_Y)
nypd$SUSPECT_REPORTED_AGE <- as.numeric(nypd$SUSPECT_REPORTED_AGE)
nypd$SUSPECT_HEIGHT <- as.numeric(nypd$SUSPECT_HEIGHT)
nypd.frisked <- glm(FRISKED_FLAG ~LOCATION_IN_OUT_CODE+ HOUR+ MONTH + YEAR + SUMMONS_ISSUED_FLAG + OTHER_CONTRABAND_FLAG+CIRCUMSTANCES_WEAPON+SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG+CIRCUMSTANCES_SURVEILLANCE+SUSPECT_HEIGHT+SUSPECT_SEX+STOP_LOCATION_PRECINCT+WEAPON+PHYSICAL_FORCE_WEAPON_IMPACT_FLAG+PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG+ PHYSICAL_FORCE_DRAW_POINT_FIREARM_FLAG+ PHYSICAL_FORCE_OTHER_FLAG, family="binomial", data=nypd)

set.seed(15)
nypdComplete <- nypd %>% na.omit
test.id <- sample(1:nrow(nypdComplete), size = .2*nrow(nypdComplete)) ## Randomly choose 20% of rows for test set
test.data <- nypdComplete[test.id,]  ## Subset to include rows designated to test set
train.data <- nypdComplete[-test.id,]  ## Exclude rows designated to test set

nypd.frisked <- cv.ncvreg(X = train.data[,c(-2,-5,-7, -9:-12, -17,-24,-25)], y = train.data$FRISKED_FLAG, penalty = "lasso")
