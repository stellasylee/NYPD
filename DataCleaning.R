library(dplyr)
library(tidyr)   
library(ggplot2)

np <- read.csv("//storage/Projects/Mat/STA310-Kuiper/Final Project/4_LeeKozikJindal_NYPD/RawData/nypd_data.csv")

# Select columns that needed for analysis
filter_variables <- c("LOCATION_IN_OUT_CODE", "OBSERVED_DURATION_MINUTES", "STOP_DURATION_MINUTES", "STOP_FRISK_TIME1", "STOP_FRISK_DATE1", 
                      "SUSPECT_ARRESTED_FLAG", "SUSPECT_ARRESTED_FLAG", "FRISKED_FLAG", "SEARCHED_FLAG", "OTHER_CONTRABAND_FLAG", "SUSPECT_RACE_DESCRIPTION", "SUSPECT_REPORTED_AGE",
                      "SUSPECT_SEX", "SUSPECT_BODY_BUILD_TYPE", "STOP_LOCATION_PRECINCT", "STOP_LOCATION_X", "STOP_LOCATION_Y", "CRIME_DESCRIPTION_SPECIFIC", "CRIME_DESCRIPTION_GENERAL")
weapon_flags <- c("KNIFE_CUTTER_FLAG", "OTHER_WEAPON_FLAG", "PHYSICAL_FORCE_WEAPON_IMPACT_FLAG", "PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG", "FIREARM_FLAG")

nypd <- dplyr::select(np, filter_variables, weapon_flags)

# Data manipulation:
nypd$LOCATION_IN_OUT_CODE <- ifelse(nypd$LOCATION_IN_OUT_CODE == 'I', 1, 
                                    ifelse(nypd$LOCATION_IN_OUT_CODE == 'O', 0, NA))
nypd$FRISKED_FLAG <- ifelse(nypd$FRISKED_FLAG == 'Y', 1, 
                            ifelse(nypd$FRISKED_FLAG == 'N', 0, NA))
nypd$SEARCHED_FLAG <- ifelse(nypd$SEARCHED_FLAG == 'Y', 1, 
                            ifelse(nypd$FRISKED_FLAG == 'N', 0, NA))

nypd$KNIFE_CUTTER_FLAG <- ifelse(nypd$KNIFE_CUTTER_FLAG == 'Y' | nypd$KNIFE_CUTTER_FLAG == '1', 1, 
                                 ifelse(nypd$KNIFE_CUTTER_FLAG == 'N', 0, NA))
nypd$PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG <- ifelse(nypd$PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG == 'Y' | nypd$PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG == '1', 1, 
                                                    ifelse(nypd$PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG == 'N', 0, NA))
nypd$PHYSICAL_FORCE_WEAPON_IMPACT_FLAG <- ifelse(nypd$PHYSICAL_FORCE_WEAPON_IMPACT_FLAG == 'Y' | nypd$PHYSICAL_FORCE_WEAPON_IMPACT_FLAG == '1', 1, 
                                                    ifelse(nypd$PHYSICAL_FORCE_WEAPON_IMPACT_FLAG == 'N', 0, NA))

nypd$SUSPECT_SEX <- ifelse(nypd$SUSPECT_SEX == 'F' | nypd$SUSPECT_SEX == 'FEMALE', 'F',
                           ifelse(nypd$SUSPECT_SEX == 'M' | nypd$SUSPECT_SEX == 'MALE', 'M',
                                  ifelse(nypd$SUSPECT_SEX == 'Z', 'Z', NA)))
nypd$SUSPECT_RACE_DESCRIPTION

nypd$SUSPECT_BODY_BUILD_TYPE

nypd$STOP_FRISK_TIME1 <- as.character(nypd$STOP_FRISK_TIME1)
nypd <- tidyr::separate(data=nypd, col=STOP_FRISK_TIME1, into = c("HOUR", "MIN"), sep = 2)
nypd$HOUR <- as.numeric(nypd$HOUR)
nypd$MIN <- as.numeric(nypd$MIN)

nypd$STOP_FRISK_DATE1 <- as.character(nypd$STOP_FRISK_DATE1)
nypd <- tidyr::separate(data=nypd, col=STOP_FRISK_DATE1, into = c("DATE", "YEAR"), sep = -4) %>%
  separate(data=., col=DATE, into = c("MONTH", "DAY"), sep= -2)
nypd$MONTH <- as.numeric(nypd$MONTH)
nypd$DAY <- as.numeric(nypd$DAY)
nypd$YEAR <- as.numeric(nypd$YEAR)

# Write the cleaned dataset
write.csv(nypd, "//storage/Projects/Mat/STA310-Kuiper/Final Project/4_LeeKozikJindal_NYPD/nypd_data.csv")
