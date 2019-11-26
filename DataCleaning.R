library(readr)
library(dplyr)
NYPD_Before_2016 <- read_csv("//storage/Projects/Mat/STA310-Kuiper/NYPD_Working/NYPD_Before_2016.csv")
nypd <- NYPD_Before_2016
nypd$inout <- ifelse(nypd$inout == "I", 1, 
                     ifelse(nypd$inout == "N", 0, NA))
nypd$explnstp <- ifelse(nypd$explnstp == 'Y', 1, 
                        ifelse(nypd$explnstp == 'N', 0, NA))
nypd$arstmade <- ifelse(nypd$arstmade == 'Y', 1, 
                        ifelse(nypd$arstmade == 'N', 0, NA))
nypd$frisked <- ifelse(nypd$frisked == 'Y', 1, 
                       ifelse(nypd$frisked == 'N', 0, NA))
nypd$searched <- ifelse(nypd$searched == 'Y', 1, 
                        ifelse(nypd$searched == 'N', 0, NA))
nypd$othpers <- ifelse(nypd$othpers == 'Y', 1, 
                       ifelse(nypd$othpers == 'N', 0, NA))
nypd$sumissue <- ifelse(nypd$sumissue == 'Y', 1, 
                        ifelse(nypd$sumissue == 'N', 0, NA))
nypd$contrabn <- ifelse(nypd$contrabn == 'Y', 1, 
                        ifelse(nypd$contrabn == 'N', 0, NA))
nypd$knifcuti <- ifelse(nypd$knifcuti == 'Y' | nypd$knifcuti == '1', 1, 
                        ifelse(nypd$knifcuti == 'N', 0, NA))
nypd$othrweap <- ifelse(nypd$othrweap == 'Y' | nypd$othrweap == '1', 1, 
                        ifelse(nypd$othrweap == 'N', 0, NA))
nypd$pf_baton <- ifelse(nypd$pf_baton == 'Y' | nypd$pf_baton == '1', 1, 
                                         ifelse(nypd$pf_baton == 'N', 0, NA))
nypd$pf_hcuff <- ifelse(nypd$pf_hcuff == 'Y' | nypd$pf_hcuff == '1', 1, 
                        ifelse(nypd$pf_hcuff == 'N', 0, NA))
nypd$pf_pepsp <- ifelse(nypd$pf_pepsp == 'Y' | nypd$pf_pepsp == '1', 1, 
                        ifelse(nypd$pf_pepsp == 'N', 0, NA))
nypd$timestop <- as.numeric(nypd$timestop)
nypd <- select(nypd, 2:5, 8:16, 20, 22, 28:30, 46:48, 50, 53, 55:63)
