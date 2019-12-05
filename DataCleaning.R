library(dplyr)
library(tidyr)   
library(ggplot2)

# FOLDERPATH
np <- read.csv(paste0(FOLDERPATH, "/nypd_data.csv"))

# Select columns that are needed for analysis
filter_variables <- c("LOCATION_IN_OUT_CODE", "OBSERVED_DURATION_MINUTES", "STOP_DURATION_MINUTES", "STOP_FRISK_TIME1", "STOP_FRISK_DATE1", 
                      "SUSPECT_ARRESTED_FLAG", "SUMMONS_ISSUED_FLAG", "FRISKED_FLAG", "SEARCHED_FLAG", "OTHER_CONTRABAND_FLAG", "CIRCUMSTANCES_WEAPON", "SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG", "BACKGROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG", 
                      "SUSPECT_RACE_DESCRIPTION", "SUSPECT_REPORTED_AGE", 'CIRCUMSTANCES_SURVEILLANCE', 'SUSPECT_HEIGHT',
                      "SUSPECT_SEX", "SUSPECT_BODY_BUILD_TYPE", "STOP_LOCATION_PRECINCT", "STOP_LOCATION_X", "STOP_LOCATION_Y", "CRIME_DESCRIPTION_SPECIFIC", "CRIME_DESCRIPTION_GENERAL")
weapon_flags <- c("KNIFE_CUTTER_FLAG", "OTHER_WEAPON_FLAG", "FIREARM_FLAG")
force_by_cop <- c("PHYSICAL_FORCE_WEAPON_IMPACT_FLAG", "PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG", 'PHYSICAL_FORCE_DRAW_POINT_FIREARM_FLAG', 'PHYSICAL_FORCE_OTHER_FLAG')
nypd <- dplyr::select(np, filter_variables, weapon_flags, force_by_cop)

# Data manipulation:
nypd$LOCATION_IN_OUT_CODE <- ifelse(nypd$LOCATION_IN_OUT_CODE == 'I', 1, 
                                    ifelse(nypd$LOCATION_IN_OUT_CODE == 'O', 0, NA))
nypd$FRISKED_FLAG <- ifelse(nypd$FRISKED_FLAG == 'Y', 1, 
                            ifelse(nypd$FRISKED_FLAG == 'N', 0, NA))
nypd$SEARCHED_FLAG <- ifelse(nypd$SEARCHED_FLAG == 'Y', 1, 
                            ifelse(nypd$SEARCHED_FLAG == 'N', 0, NA))
nypd$SUMMONS_ISSUED_FLAG <- ifelse(nypd$SUMMONS_ISSUED_FLAG == 'Y' | nypd$SUMMONS_ISSUED_FLAG == '1', 1, 
                                 ifelse(nypd$SUMMONS_ISSUED_FLAG == 'N', 0, NA))
nypd$KNIFE_CUTTER_FLAG <- ifelse(nypd$KNIFE_CUTTER_FLAG == 'Y' | nypd$KNIFE_CUTTER_FLAG == '1', 1, 
                                 ifelse(nypd$KNIFE_CUTTER_FLAG == 'N', 0, NA))
nypd$SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG <- ifelse(nypd$SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG == 'Y' | nypd$SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG == '1', 1, 
                                 ifelse(nypd$SUSPECTS_ACTIONS_DRUG_TRANSACTIONS_FLAG == 'N', 0, NA))
nypd$PHYSICAL_FORCE_WEAPON_IMPACT_FLAG <- ifelse(nypd$PHYSICAL_FORCE_WEAPON_IMPACT_FLAG == 'Y' | nypd$PHYSICAL_FORCE_WEAPON_IMPACT_FLAG == '1', 1, 
                                 ifelse(nypd$PHYSICAL_FORCE_WEAPON_IMPACT_FLAG == 'N', 0, NA))
nypd$PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG <- ifelse(nypd$PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG == 'Y' | nypd$PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG == '1', 1, 
                                 ifelse(nypd$PHYSICAL_FORCE_HANDCUFF_SUSPECT_FLAG == 'N', 0, NA))
nypd$BACKGROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG <- ifelse(nypd$BACKGROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG == 'Y' | nypd$BACKGROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG == '1', 1, 
                                                           ifelse(nypd$BACKGROUND_CIRCUMSTANCES_VIOLENT_CRIME_FLAG == 'N', 0, NA))
nypd$SUSPECT_ARRESTED_FLAG <- ifelse(nypd$SUSPECT_ARRESTED_FLAG == 'Y' | nypd$SUSPECT_ARRESTED_FLAG == '1', 1, 
                                 ifelse(nypd$SUSPECT_ARRESTED_FLAG == 'N', 0, NA))
nypd$OTHER_CONTRABAND_FLAG <- ifelse(nypd$OTHER_CONTRABAND_FLAG == 'Y' | nypd$OTHER_CONTRABAND_FLAG == '1', 1, 
                                     ifelse(nypd$OTHER_CONTRABAND_FLAG == 'N', 0, NA))

nypd$SUSPECT_SEX <- ifelse(nypd$SUSPECT_SEX == 'F' | nypd$SUSPECT_SEX == 'FEMALE', 'F',
                           ifelse(nypd$SUSPECT_SEX == 'M' | nypd$SUSPECT_SEX == 'MALE', 'M',
                                  ifelse(nypd$SUSPECT_SEX == 'Z', 'Z', NA)))

nypd$STOP_FRISK_TIME1 <- as.character(nypd$STOP_FRISK_TIME1)
nypd <- tidyr::separate(data=nypd, col=STOP_FRISK_TIME1, into = c("HOUR", "MIN"), sep = -2)
nypd$HOUR <- as.numeric(nypd$HOUR)
nypd$MIN <- as.numeric(nypd$MIN)

nypd$STOP_FRISK_DATE1 <- as.character(nypd$STOP_FRISK_DATE1)
nypd <- tidyr::separate(data=nypd, col=STOP_FRISK_DATE1, into = c("DATE", "YEAR"), sep = -4) %>%
  separate(data=., col=DATE, into = c("MONTH", "DAY"), sep= -2)
nypd$MONTH <- as.numeric(nypd$MONTH)
nypd$DAY <- as.numeric(nypd$DAY)
nypd$YEAR <- as.numeric(nypd$YEAR)

nypd$SUSPECT_RACE_DESCRIPTION <- as.character(nypd$SUSPECT_RACE_DESCRIPTION)
nypd$SUSPECT_RACE_DESCRIPTION <- ifelse(nypd$SUSPECT_RACE_DESCRIPTION == "A" | nypd$SUSPECT_RACE_DESCRIPTION == "ASIAN / PACIFIC ISLANDER" |
                                          nypd$SUSPECT_RACE_DESCRIPTION == "ASIAN/PAC.ISL", "Asian",
                                        ifelse(nypd$SUSPECT_RACE_DESCRIPTION == "B" | nypd$SUSPECT_RACE_DESCRIPTION == "BLACK", "Black",
                                               ifelse(nypd$SUSPECT_RACE_DESCRIPTION == "I" | nypd$SUSPECT_RACE_DESCRIPTION == "AMER IND" |
                                                        nypd$SUSPECT_RACE_DESCRIPTION == "AMERICAN INDIAN/ALASKAN NATIVE", "American Indian",
                                                      ifelse(nypd$SUSPECT_RACE_DESCRIPTION == "P" | nypd$SUSPECT_RACE_DESCRIPTION == "BLACK HISPANIC", "Black Hispanic",
                                                             ifelse(nypd$SUSPECT_RACE_DESCRIPTION == "Q" | nypd$SUSPECT_RACE_DESCRIPTION == "WHITE HISPANIC", "White Hispanic",
                                                                    ifelse(nypd$SUSPECT_RACE_DESCRIPTION == "W" | nypd$SUSPECT_RACE_DESCRIPTION == "WHITE", "White", "Other"))))))

nypd$SUSPECT_BODY_BUILD_TYPE <- ifelse(nypd$SUSPECT_BODY_BUILD_TYPE == 'H' | nypd$SUSPECT_BODY_BUILD_TYPE == 'HEA', "HEAVY",
                                       ifelse(nypd$SUSPECT_BODY_BUILD_TYPE == 'M' | nypd$SUSPECT_BODY_BUILD_TYPE == 'U' | nypd$SUSPECT_BODY_BUILD_TYPE == 'MED', "MEDIUM",
                                              ifelse(nypd$SUSPECT_BODY_BUILD_TYPE == 'T' | nypd$SUSPECT_BODY_BUILD_TYPE == 'THN', "THIN", "Unknown")))

nypd$WEAPON <- ifelse(nypd$FIREARM_FLAG == 1 | nypd$OTHER_WEAPON_FLAG == 1 | nypd$KNIFE_CUTTER_FLAG == 1, 1,
                      ifelse(nypd$FIREARM_FLAG == 0 | nypd$OTHER_WEAPON_FLAG == 0 | nypd$KNIFE_CUTTER_FLAG == 0, 0, NA))

# Write the cleaned dataset
write.csv(nypd, "nypd_data.csv")
